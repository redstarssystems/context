(ns org.rssys.context.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [matcho.core :refer [match]]
            [org.rssys.context.core :as sut]
            [unifier.response :as r]
            [clojure.spec.alpha :as s])
  (:import (clojure.lang Atom)))

(deftest get-component-test

  (let [*ctx          (atom {})
        new-component {:id :abc :config {}}]
    (testing "get normal component value"
      (sut/create! *ctx new-component)
      (match (sut/get-component *ctx :abc) new-component)
      (match (sut/get-component-value @*ctx :abc) new-component)
      (match (-> @*ctx (get-in (conj sut/*components-path-vec* :abc))) new-component))) ;; manual check

  (let [*ctx (atom {})]
    (testing "get with empty values"
      (match (sut/get-component nil :abc) nil?)
      (match (sut/get-component *ctx :abc) nil?))))

(deftest create!-test

  (let [*ctx (atom {})]
    (testing "do not create component with invalid structure"
      (is (thrown? AssertionError (sut/create! *ctx {:id 1})))
      (is (thrown? AssertionError (sut/create! *ctx {:id :web :config {} :start-fn 1})))))

  (let [*ctx          (atom {})
        new-component {:id :abc :config {}}]
    (testing "normal create"
      (sut/create! *ctx new-component)
      (match (-> @*ctx (get-in (conj sut/*components-path-vec* :abc))) new-component) ;; manual check
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :abc)))) :stopped)
      (match (sut/create! *ctx {:id :web :config {}}) r/success?)
      (match (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [state-obj])}) r/success?)))

  (let [*ctx (atom {})]
    (testing "id conflict detection"
      (sut/create! *ctx {:id :web :config {}})
      (match (sut/create! *ctx {:id :web :config {}}) r/error?)
      (match (:type (sut/create! *ctx {:id :web :config {}})) ::r/conflict))))


(deftest update!-test

  (let [*ctx (atom {})]
    (testing "normal update"
      (sut/create! *ctx {:id :web :config {}})
      (match (sut/update! *ctx {:id :web :config {:a 1 :b 2}}) r/success?)
      (match (-> @*ctx (get-in (conj sut/*components-path-vec* :web))) {:id :web :config {:a 1 :b 2}}) ;; manual check
      (match (sut/update! *ctx {:id :web :config {:a 1 :b 2} :start-deps [:db]}) r/success?)
      (match (sut/update! *ctx {:id :web :config {} :start-fn (fn [config]) :stop-fn (fn [state-obj])}) r/success?)))

  (let [*ctx (atom {})]
    (testing "do not update component with invalid structure"
      (sut/create! *ctx {:id :web :config {}})
      (is (thrown? AssertionError (sut/update! *ctx {:id :web :config {:a 1 :b 2} :start-deps 1})))))

  (let [*ctx (atom {})]
    (testing "do not update if component is not found"
      (sut/create! *ctx {:id :web :config {}})
      (match (sut/update! *ctx {:id :web2 :config {:a 1 :b 2}}) r/error?)
      (match (:type (sut/update! *ctx {:id :web2 :config {:a 1 :b 2}}) ::r/not-found)))))

(deftest delete!-test

  (let [*ctx          (atom {})
        new-component {:id :web :config {:b 2}}]

    (testing "normal delete"
      (sut/create! *ctx new-component)
      (match (-> @*ctx (get-in (conj sut/*components-path-vec* :web))) new-component)
      (let [result (sut/delete! *ctx :web)]
        (match (-> @*ctx (get-in (conj sut/*components-path-vec* :web))) nil?) ;; manual check
        (match result r/success?)
        (match (:type result) ::r/deleted))))

  (let [*ctx (atom {})]
    (testing "do not delete if component is not found"
      (sut/create! *ctx {:id :web :config {}})
      (match (sut/delete! *ctx :web2) r/error?)
      (match (:type (sut/delete! *ctx :web2) ::r/not-found)))))


(deftest set-config!-test

  (let [*ctx (atom {})]
    (testing "normal config update"
      (sut/create! *ctx {:id :web :config {}})
      (let [new-config {:a 4 :b 2}
            result     (sut/set-config! *ctx :web new-config)]
        (match (:config (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) new-config) ;; manual check
        (match result r/success?))))

  (let [*ctx (atom {})]
    (testing "do not update config if component is not found"
      (sut/create! *ctx {:id :web :config {}})
      (match (sut/set-config! *ctx :web2 {:a 1 :b 7}) r/error?))))


(deftest start!-test

  (let [*ctx (atom {})]
    (testing "normal start"
      (sut/create! *ctx {:id :web :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :db :config {:db 42}})         ;; define minimal component w/o start/stop fn's
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :stopped) ;; manual check
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) :stopped) ;; manual check
      (let [result-web (sut/start! *ctx :web)
            ;; define start/stop fn's in runtime
            result-db  (sut/start! *ctx :db (fn [config] config) (fn [obj-state]))]
        (match result-web r/success?)
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :started) ;; manual check
        (match (:data result-web) :web)

        (match result-db r/success?)
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) :started) ;; manual check
        ;; check that config is passed during start
        (match (:state-obj (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) {:db 42})
        (match (:data result-db) :db))))

  (let [*ctx (atom {})]
    (testing "start with dependencies"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [:db :cache] :start-fn (fn [config]) :stop-fn (fn [obj-state])})

      (let [result-web (sut/start! *ctx :web)]
        (match result-web r/success?)
        (match (:data result-web) :web)
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :cache)))) :started) ;; manual check
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) :started)
        (match (:stop-deps (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) [:web]))))

  (let [*ctx (atom {})]
    (testing "do not start if there are problems with dependencies"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {}})
      (sut/create! *ctx {:id :web :config {} :start-deps [:db :cache] :start-fn (fn [config]) :stop-fn (fn [obj-state])})

      (let [result-web (sut/start! *ctx :web)]
        (match result-web r/error?)
        (match (:data result-web) :web)
        (match (-> result-web :meta :deps) [:cache])        ;; check if result contains problem components
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :stopped) ;; manual check
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :cache)))) :stopped) ;; manual check
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) :started))))

  (let [*ctx (atom {})]
    (testing "minimal component cannot be run without start/stop functions"
      (sut/create! *ctx {:id :web :config {}})
      (let [result (sut/start! *ctx :web)]
        (match result r/error?))))

  (let [*ctx (atom {})]
    (testing "do not start if component is not found"
      (sut/create! *ctx {:id :web :config {}})
      (let [result (sut/start! *ctx :db)]
        (match result r/error?)
        (match (:type result) ::r/not-found))))

  (let [*ctx (atom {})]
    (testing "do not start if component is already started"
      (sut/create! *ctx {:id :web :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (let [result  (sut/start! *ctx :web)
            result2 (sut/start! *ctx :web)]
        (match result r/success?)
        (match (:data result) :web)
        (match (:meta result) (complement string?))
        (match result2 r/success?)
        (match (:meta result2) string?))))

  )

(deftest stop!-test

  (let [*ctx (atom {})]
    (testing "normal stop"
      (sut/create! *ctx {:id :web :config {:a 42} :start-fn (fn [config] config) :stop-fn (fn [obj-state] obj-state)})
      (sut/start! *ctx :web)
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :started) ;; manual check
      (let [result-web (sut/stop! *ctx :web)]
        (match result-web r/success?)
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :stopped) ;; manual check
        (match (:state-obj (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) {:a 42}) ;; manual check
        (match (:data result-web) :web))))

  (let [*ctx (atom {})]
    (testing "stop with dependencies"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-deps [:db] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [:cache] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/start! *ctx :web)
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) :started)
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :cache)))) :started)
      (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :started)

      (let [result-db (sut/stop! *ctx :db)]
        (match result-db r/success?)
        (match (:data result-db) :db)
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :db)))) :stopped) ;; manual check
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :cache)))) :stopped) ;; manual check
        (match (:status (-> @*ctx (get-in (conj sut/*components-path-vec* :web)))) :stopped))))

  (let [*ctx (atom {})]
    (testing "do not stop if component is not found"
      (sut/create! *ctx {:id :web :config {}})
      (let [result (sut/start! *ctx :db)]
        (match result r/error?)
        (match (:type result) ::r/not-found))))

  (let [*ctx (atom {})]
    (testing "do not stop if component is already stopped"
      (sut/create! *ctx {:id :web :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/start! *ctx :web)
      (let [result  (sut/stop! *ctx :web)
            result2 (sut/stop! *ctx :web)]
        (match result r/success?)
        (match (:data result) :web)
        (match (:meta result) (complement string?))
        (match result2 r/success?)
        (match (:meta result2) string?)))))

(deftest list-all-ids-test

  (let [*ctx (atom {})]
    (testing "list all registered components id"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-deps [:db] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [:cache] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (match (sut/list-all-ids *ctx) [:db :cache :web])
      (sut/delete! *ctx :cache)
      (match (sut/list-all-ids *ctx) [:db :web]))))

(deftest started?-stopped?-test

  (let [*ctx (atom {})]
    (testing "check if component is started? / stopped?"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (match (sut/stopped? *ctx :db))
      (sut/start! *ctx :db)
      (match (sut/started? *ctx :db))
      (sut/stop! *ctx :db)
      (match (sut/stopped? *ctx :db)))))

(deftest started-stopped-ids-test

  (let [*ctx (atom {})]
    (testing "check list of started and stopped components"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-deps [] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/start! *ctx :db)
      (sut/start! *ctx :web)
      (match (sut/started-ids *ctx) [:db :web])
      (match (sut/stopped-ids *ctx) [:cache]))))

(deftest start-some-stop-some-test

  (let [*ctx (atom {})]
    (testing "start some / stop some components"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-deps [] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/start-some *ctx [:db :web])
      (match (sut/started-ids *ctx) [:db :web])
      (match (sut/stopped-ids *ctx) [:cache])
      (sut/stop-some *ctx [:db :web])
      (match (sut/stopped-ids *ctx) [:db :cache :web])))

  (let [*ctx (atom {})]
    (testing "start some components with dependency problems"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-deps [:db] :start-fn (fn [config] (/ 1 0)) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [:cache] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (let [result (sut/start-some *ctx [:web])]
        (match (sut/started-ids *ctx) [:db])
        (match (sut/stopped-ids *ctx) [:cache :web])
        (match (:data result) :web)
        (match (:meta result) string?)
        ))))

(deftest start-all-stop-all-test

  (let [*ctx (atom {})]
    (testing "start all / stop all components"
      (sut/create! *ctx {:id :db :config {} :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :cache :config {} :start-deps [] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/create! *ctx {:id :web :config {} :start-deps [] :start-fn (fn [config]) :stop-fn (fn [obj-state])})
      (sut/start-all *ctx)
      (match (sut/started-ids *ctx) [:db :cache :web])
      (sut/stop-all *ctx)
      (match (sut/stopped-ids *ctx) [:db :cache :web]))))


(deftest build-context-test
  (testing "build system context from system map"
    (let [*ctx       (atom {})
          system-map [
                      {:id         :cfg                     ;; cfg component will prepare config for all context
                       :config     {}
                       :start-deps []
                       :start-fn   (fn [config]
                                     (println "reading config data from OS & JVM environment variables or config file")
                                     {:db    {:host "localhost" :port 1234 :user "sa" :password "*****"}
                                      :cache {:host "127.0.0.1" :user "cache-user" :pwd "***"}
                                      :web   {:host "localhost" :port 8080 :root-context "/main"}})
                       :stop-fn    (fn [obj-state])}

                      {:id         :db
                       :config     (fn [ctx] (-> (sut/get-component-value ctx :cfg) :state-obj :db))
                       :start-deps [:cfg]
                       :start-fn   (fn [config] (println "starting db" :config config))
                       :stop-fn    (fn [obj-state] (println "stopping db..."))}

                      {:id         :cache
                       :config     (fn [ctx] (-> (sut/get-component-value ctx :cfg) :state-obj :cache))
                       :start-deps [:cfg :db]
                       :start-fn   (fn [config] (println "starting cache" :config config))
                       :stop-fn    (fn [obj-state] (println "stopping cache..."))}

                      {:id         :log
                       :config     {:output "stdout"}
                       :start-deps []
                       :start-fn   (fn [config] (println "starting logging" :config config))
                       :stop-fn    (fn [obj-state] (println "stopping logging..."))}

                      {:id         :web
                       :config     (fn [ctx] (-> (sut/get-component-value ctx :cfg) :state-obj :web))
                       :start-deps [:cfg :db :cache :log]
                       :start-fn   (fn [config]
                                     (println "starting web" :config config)
                                     (println "pass the whole context to web handler:" *ctx))
                       :stop-fn    (fn [obj-state] (println "stopping web..."))}
                      ]
          ]
      (sut/build-context *ctx system-map)
      (is (instance? Atom *ctx))
      (match (sut/list-all-ids *ctx) [:cfg :db :cache :log :web])
      (match (sut/stopped-ids *ctx) [:cfg :db :cache :log :web])
      (sut/start-all *ctx)
      (match (sut/started-ids *ctx) [:cfg :db :cache :log :web])
      (match (-> (sut/get-component *ctx :cache) :config) {:host "127.0.0.1" :user "cache-user" :pwd "***"})
      (match (-> (sut/get-component *ctx :log) :config) {:output "stdout"})
      )))

(deftest isolated-start!-stop!-test
  (testing "isolated stop & start using multiple components"
    (let [*ctx       (atom {})
          system-map [
                      {:id         :cfg                     ;; cfg component will prepare config for all context
                       :config     {}
                       :start-deps []
                       :start-fn   (fn [config]
                                     (println "reading config data from OS & JVM environment variables or config file")
                                     {:db    {:host "localhost" :port 1234 :user "sa" :password "*****"}
                                      :cache {:host "127.0.0.1" :user "cache-user" :pwd "***"}
                                      :web   {:host "localhost" :port 8080 :root-context "/main"}})
                       :stop-fn    (fn [obj-state])}

                      {:id         :db
                       :config     (fn [ctx] (-> (sut/get-component-value ctx :cfg) :state-obj :db))
                       :start-deps [:cfg]
                       :start-fn   (fn [config] (println "starting db" :config config))
                       :stop-fn    (fn [obj-state] (println "stopping db..."))}

                      {:id         :cache
                       :config     (fn [ctx] (-> (sut/get-component-value ctx :cfg) :state-obj :cache))
                       :start-deps [:cfg :db]
                       :start-fn   (fn [config] (println "starting cache" :config config))
                       :stop-fn    (fn [obj-state] (println "stopping cache..."))}

                      {:id         :log
                       :config     {:output "stdout"}
                       :start-deps []
                       :start-fn   (fn [config] (println "starting logging" :config config))
                       :stop-fn    (fn [obj-state] (println "stopping logging..."))}

                      {:id         :web
                       :config     (fn [ctx] (-> (sut/get-component-value ctx :cfg) :state-obj :web))
                       :start-deps [:cfg :db :cache :log]
                       :start-fn   (fn [config]
                                     (println "starting web" :config config)
                                     (println "pass the whole context to web handler:" *ctx))
                       :stop-fn    (fn [obj-state] (println "stopping web..."))}
                      ]
          ]
      (sut/build-context *ctx system-map)
      (sut/start-all *ctx)

      (match (sut/started-ids *ctx) [:cfg :db :cache :log :web])

      (sut/isolated-stop! *ctx :cache)
      (match (sut/stopped? *ctx :cache) true)
      (match (sut/stopped? *ctx :web) false)
      (match (:stop-deps (sut/get-component *ctx :cache)) [:web])
      (sut/isolated-start! *ctx :cache)
      (match (sut/stopped? *ctx :cache) false)
      (match (sut/stopped? *ctx :web) false)
      (match (:stop-deps (sut/get-component *ctx :cache)) [:web])

      (sut/isolated-stop! *ctx :db)
      (match (sut/stopped? *ctx :db) true)
      (match (sut/stopped? *ctx :web) false)
      (match (sut/stopped? *ctx :cache) false)
      (match (:stop-deps (sut/get-component *ctx :db)) [:cache :web])
      (sut/isolated-start! *ctx :db)
      (match (sut/started? *ctx :db) true)
      (match (sut/started? *ctx :cache) true)
      (match (sut/started? *ctx :web) true)
      (match (:stop-deps (sut/get-component *ctx :db)) [:cache :web])))
  )
