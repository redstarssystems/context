(ns ^{:author "Mikhail Ananev"}
  org.rssys.context.core
  (:require [unifier.response :as r]
            [clojure.spec.alpha :as s])
  (:import (clojure.lang Keyword Atom IFn)))

;; component's specification
(s/def ::id keyword?)                                       ;; component's identifier
(s/def ::config (s/or :map map? :fn ifn?))                  ;; config is a map or fn with 1 arg - the current context value
(s/def ::state-obj any?)                                    ;; a stateful object
(s/def ::status #{:started :stopped :disabled})             ;; status
(s/def ::start-deps (s/coll-of keyword?))                   ;; dependencies which should be started before this component
(s/def ::stop-deps (s/coll-of keyword?))                    ;; dependencies which should be stopped before this component
(s/def ::start-fn ifn?)                                     ;; function which starts this component
(s/def ::stop-fn ifn?)                                      ;; function which stops this component

(s/def ::component (s/keys
                     :req-un [::id ::config]
                     :opt-un [::state-obj ::status ::start-deps ::stop-deps ::start-fn ::stop-fn]))

(defrecord EmptyState [])                                   ;; default value for stopped components

(def ^:dynamic *stopped-component-state*
  "Special value, which indicates that state is empty or stopped."
  EmptyState)

(def ^:dynamic *component-disabled*
  "Special config value, which prevents a component to start."
  :context/component-disabled)

(def ^:dynamic *components-path-vec*
  "A path in the system context where reside components (stateful objects)"
  [:context/components])

(def ^:dynamic *print-exceptions?*
  "Print exception info to stdout during start/stop operations with components."
  true)

(def ^:dynamic *ignore-cyclic-deps?*
  "If false then throw Exception if cyclic dependency is detected. If true, ignore cyclic dependency loop and force to start."
  false)

(defn- dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn get-component
  "Returns: `component state` from system context using its `id-kwd`."
  [^Atom *ctx ^Keyword id-kwd] (when (and *ctx id-kwd) (-> @*ctx (get-in (conj *components-path-vec* id-kwd)))))

(defn get-component-value
  "Returns: `component value` from system context value using its `id-kwd`"
  [ctx ^Keyword id-kwd] (when (and ctx id-kwd) (-> ctx (get-in (conj *components-path-vec* id-kwd)))))

(defn create!
  "Create new component in the system context.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx new-component]
  {:pre [(s/valid? ::component new-component)]}
  (let [new-component (assoc new-component
                        :state-obj nil
                        :status :stopped
                        :start-deps (or (:start-deps new-component) []))]
    (if (nil? (get-component *ctx (:id new-component)))
      (do
        (swap! *ctx assoc-in (conj *components-path-vec* (:id new-component)) new-component)
        (r/as-created (:id new-component)))
      (r/as-conflict (:id new-component) "component with such id is already exist"))))

(defn update!
  "Update a component's state in the system context.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx new-component-state]
  {:pre [(s/valid? ::component new-component-state)]}
  (if-let [state-obj (get-component *ctx (:id new-component-state))]
    (do (swap! *ctx assoc-in (conj *components-path-vec* (:id new-component-state)) new-component-state)
        (r/as-success (:id new-component-state)))
    (r/as-not-found (:id new-component-state) "no such id in the context")))

(defn delete!
  "Delete a component from the system context.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx ^Keyword id-kwd]
  (if-let [state-obj (get-component *ctx id-kwd)]
    (if-not (= :started (:status state-obj))
      (do (swap! *ctx dissoc-in (conj *components-path-vec* id-kwd))
          (r/as-deleted id-kwd))
      (r/as-busy id-kwd "stop the component before delete"))
    (r/as-not-found id-kwd "no such id in the context")))

(defn- set-stop-dep!
  "Set dependency to be stopped in case of this component stops.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx ^Keyword id-kwd ^Keyword dep-to-be-stopped-kwd]
  (let [state-obj (get-component *ctx id-kwd)]
    (update! *ctx (assoc state-obj
                    :stop-deps (into [] (into #{} (conj (:stop-deps state-obj) dep-to-be-stopped-kwd)))))))

(defn set-config!
  "Set new config for a component.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx ^Keyword id-kwd new-config]
  (if-let [state-obj (get-component *ctx id-kwd)]
    (update! *ctx (assoc state-obj :config new-config))
    (r/as-not-found id-kwd "no such id in the context")))

(defn- start-component! [*ctx state-obj start-fn stop-fn]
  (r/safe
    (let [config (if (map? (:config state-obj))
                   (:config state-obj)
                   ((:config state-obj) @*ctx))]
      (update! *ctx (assoc state-obj :config config))       ;; config should be evaluated at the start moment
      (cond

        (= (get config *component-disabled*) true) (update! *ctx (assoc state-obj
                                                                      :config config
                                                                      :state-obj *stopped-component-state*
                                                                      :status :disabled
                                                                      :start-fn start-fn
                                                                      :stop-fn stop-fn))


        :else (update! *ctx (assoc state-obj
                              :config config
                              :state-obj (start-fn config)
                              :status :started
                              :start-fn start-fn
                              :stop-fn stop-fn))))
    (fn [ex]
      (when *print-exceptions?*
        (println (format "\nstart exception [%s]: %s,\n%s\ndetails: \n%s\n"
                   (:id state-obj) (ex-message ex) (apply str (repeat 50 "-")) (ex-data ex))))
      (r/as-exception (:id state-obj) {:msg (ex-message ex) :cause (ex-cause ex)}))))

(defn- stop-component! [*ctx state-obj]
  (r/safe
    (cond
      (= (:status state-obj) :disabled) (update! *ctx (assoc state-obj
                                                        :state-obj *stopped-component-state*
                                                        :stop-deps []
                                                        :status :stopped))

      :else (update! *ctx (assoc state-obj
                            :state-obj (do ((:stop-fn state-obj) (:state-obj state-obj)) *stopped-component-state*)
                            :stop-deps []
                            :status :stopped)))

    (fn [ex]
      (when *print-exceptions?*
        (println (format "\nstop exception [%s]: %s,\n%s\ndetails: \n%s\n"
                   (:id state-obj) (ex-message ex) (apply str (repeat 50 "-")) (ex-data ex))))
      (r/as-exception (:id state-obj) {:msg (ex-message ex) :cause (ex-cause ex)}))))

(declare isolated-start!)



(defn start!
  "Start a component and all its dependencies using given `id-kwd` and (optionally) the start/stop functions.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  ([^Atom *ctx ^Keyword id-kwd]
   (let [{:keys [start-fn stop-fn]} (get-component *ctx id-kwd)]
     (start! *ctx id-kwd start-fn stop-fn)))
  ([^Atom *ctx ^Keyword id-kwd dep-path-list]
   (let [{:keys [start-fn stop-fn]} (get-component *ctx id-kwd)]
     (start! *ctx id-kwd start-fn stop-fn dep-path-list)))
  ([^Atom *ctx ^Keyword id-kwd ^IFn start-fn ^IFn stop-fn]
   (start! *ctx id-kwd start-fn stop-fn []))
  ([^Atom *ctx ^Keyword id-kwd ^IFn start-fn ^IFn stop-fn dep-path-list]
   (if-let [state-obj (get-component *ctx id-kwd)]
     (if (and (ifn? start-fn) (ifn? stop-fn))
       (if (= :started (:status state-obj))
         (r/as-success id-kwd "already started")
         (if-let [start-deps (:start-deps state-obj)]
           (let [deps-states      (mapv #(get-component *ctx %) start-deps)
                 not-started-deps (filterv #(not= :started (:status %)) deps-states)]
             (if (seq not-started-deps)
               (if (some #{id-kwd} dep-path-list)
                 (if-not *ignore-cyclic-deps?*
                   (throw (ex-info (str "detected cyclic dependency: " dep-path-list) {id-kwd dep-path-list}))
                   (isolated-start! *ctx id-kwd start-fn stop-fn))
                 (let [not-started-result (reduce (fn [acc i]
                                                    (if-not (r/success? (start! *ctx (:id i) (conj dep-path-list id-kwd)))
                                                      (conj acc (:id i))
                                                      (do (set-stop-dep! *ctx (:id i) (:id state-obj)) nil)))
                                            []
                                            not-started-deps)]
                   (if (seq not-started-result)
                     (r/as-error id-kwd {:msg "can't start these dependencies" :deps not-started-result})
                     (let [deps-states'      (mapv #(get-component *ctx %)  (:start-deps state-obj))
                           disabled-deps (mapv :id (filterv #(= :disabled (:status %)) deps-states'))]
                       (if (seq disabled-deps)
                         (start-component! *ctx (assoc state-obj :config {*component-disabled* true :disabled-deps disabled-deps}) start-fn stop-fn) ;;inject disabled configuration due to disabled deps
                         (start-component! *ctx state-obj start-fn stop-fn))))))
               (do
                 (run! #(set-stop-dep! *ctx % id-kwd) (mapv :id deps-states))
                 (start-component! *ctx state-obj start-fn stop-fn))))
           (start-component! *ctx state-obj start-fn stop-fn)))
       (r/as-error id-kwd "start/stop functions are not defined for this component"))
     (r/as-not-found id-kwd "no such id in the context"))))

(defn isolated-start!
  "Isolated start a component ignoring all its dependencies, using given `id-kwd` and (optionally) the start/stop functions.
  Preserving :stops-deps value from previous isolated stop or start.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  ([^Atom *ctx ^Keyword id-kwd]
   (let [{:keys [start-fn stop-fn]} (get-component *ctx id-kwd)]
     (start! *ctx id-kwd start-fn stop-fn)))
  ([^Atom *ctx ^Keyword id-kwd ^IFn start-fn ^IFn stop-fn]
   (if-let [state-obj (get-component *ctx id-kwd)]
     (let [stop-deps (:stop-deps state-obj)]
       (if (and (ifn? start-fn) (ifn? stop-fn))
         (if (= :started (:status state-obj))
           (r/as-success id-kwd "already started")
           (do
             (start-component! *ctx state-obj start-fn stop-fn)
             (update! *ctx (assoc (get-component *ctx id-kwd) :stop-deps (or stop-deps [])))))
         (r/as-error id-kwd "start/stop functions are not defined for this component")))
     (r/as-not-found id-kwd "no such id in the context"))))

(defn stop!
  "Stop the component and all its dependencies using given `id-kwd`.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx ^Keyword id-kwd]
  (if-let [state-obj (get-component *ctx id-kwd)]
    (if (not= :started (:status state-obj))
      (r/as-success id-kwd "already stopped")
      (if-let [to-be-stopped-deps (:stop-deps state-obj)]
        (let [deps-states      (mapv #(get-component *ctx %) to-be-stopped-deps)
              not-stopped-deps (reduce (fn [acc i]
                                         (when-not (r/success? (stop! *ctx (:id i)))
                                           (conj acc (:id i))))
                                 []
                                 deps-states)]
          (if (seq not-stopped-deps)
            (r/as-error id-kwd {:msg "can't stop these dependencies" :deps not-stopped-deps})
            (stop-component! *ctx state-obj)))
        (stop-component! *ctx state-obj)))
    (r/as-not-found id-kwd "no such id in the context")))

(defn isolated-stop!
  "Isolated stop the component ignoring all its dependencies using given `id-kwd`.
  Preserving :stops-deps value from previous start.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx ^Keyword id-kwd]
  (if-let [state-obj (get-component *ctx id-kwd)]
    (let [stop-deps (:stop-deps state-obj)]
      (if (not= :started (:status state-obj))
        (r/as-success id-kwd "already stopped")
        (do
          (stop-component! *ctx state-obj)
          (update! *ctx (assoc (get-component *ctx id-kwd) :stop-deps (or stop-deps []))))
        ))
    (r/as-not-found id-kwd "no such id in the context")))

(defn list-all-ids
  "Get list of ids for all registered components.
   Returns:
    * vector of keywords."
  [^Atom *ctx] (into [] (keys (get-in @*ctx *components-path-vec*))))

(defn started?
  "Check if the component is started."
  [^Atom *ctx ^Keyword id-kwd] (= :started (:status (get-component *ctx id-kwd))))

(def stopped? "Check if the component is stopped." (complement started?))

(defn disabled?
  "Check if the component is disabled."
  [^Atom *ctx ^Keyword id-kwd] (= :disabled (:status (get-component *ctx id-kwd))))

(defn disabled-ids
  "Get list of ids for all disabled components.
   Returns:
    * vector of keywords."
  [^Atom *ctx] (filterv #(disabled? *ctx %) (list-all-ids *ctx)))

(defn started-ids
  "Get list of ids for all started components.
   Returns:
    * vector of keywords."
  [^Atom *ctx] (filterv #(started? *ctx %) (list-all-ids *ctx)))

(defn stopped-ids
  "Get list of ids for all stopped components.
   Returns:
    * vector of keywords."
  [^Atom *ctx] (filterv #(stopped? *ctx %) (list-all-ids *ctx)))

(defn start-some
  "Start some of the registered components, which are not started yet.
   Params:
    * `component-list` - vector of keywords with component's id.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx component-list]
  (let [result (reduce (fn [acc i]
                         (if-not (r/success? (start! *ctx i))
                           (reduced (r/as-error i "can't start the component"))
                           (conj acc i)))
                 []
                 component-list)]
    (if (r/error? result)
      result
      (r/as-success result))))

(defn stop-some
  "Stop some of the registered components, which are not stopped yet.
   Params:
    * `component-list` - vector of keywords with component's id.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx component-list]
  (let [result (reduce (fn [acc i]
                         (if-not (r/success? (stop! *ctx i))
                           (reduced (r/as-error i "can't stop the component"))
                           (conj acc i)))
                 []
                 component-list)]
    (if (r/error? result)
      result
      (r/as-success result))))

(defn start-all
  "Start all registered components, which are not started yet.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx]
  (start-some *ctx (stopped-ids *ctx)))

(defn stop-all
  "Stop all registered components, which are not stopped yet.
   Returns:
    * `r/success-types` - if success.
    * `r/error-types`   - if failure."
  [^Atom *ctx]
  (stop-some *ctx (started-ids *ctx)))

(defn build-context
  "Build a system context using given `component-list`.
   Params:
    * `*new-ctx`       - atom with map (empty or not), where to put result.
    * `component-list` - vector of ::component
   Returns:
    * `*new-ctx`  - modified atom as system context if success.
    * `exception` - if component-list is invalid or other errors."
  [*new-ctx component-list]
  (let [config-spec (s/coll-of ::component)]
    (if (s/valid? config-spec component-list)
      (if (apply distinct? (mapv :id component-list))
        (reduce (fn [acc i] (if (r/success? (create! acc i)) acc
                                                             (throw (ex-info "can't create the component" i))))
          *new-ctx component-list)
        ((throw (ex-info "component-list contains duplicate :id" {:explain-data (mapv :id component-list)}))))
      (throw (ex-info "component-list is not valid" {:explain-data (s/explain config-spec component-list)})))))
