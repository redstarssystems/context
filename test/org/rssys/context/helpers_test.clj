(ns org.rssys.context.helpers-test
  (:require [clojure.test :refer :all]
            [org.rssys.context.helpers :as sut]
            [org.rssys.context.core :as core]
            [matcho.core :refer [match not-match]]))

(deftest config-value-test
  (let [*ctx (atom {})]
    (core/create! *ctx {:id :db :config {:a 1} :start-fn (fn [config] 11) :stop-fn (fn [obj-state])})
    (core/start-all *ctx)
    (testing "get config value"
      (match (sut/config-value *ctx :db) {:a 1})
      (match (sut/config-value @*ctx :db) {:a 1}))
    (core/stop-all *ctx)))

(deftest state-value-test
  (let [*ctx (atom {})
        p (promise)]
    (core/create! *ctx {:id :db :config {:a 1} :start-fn (fn [config] 11) :stop-fn (fn [obj-state])})
    (core/create! *ctx {:id :cache :config {:b 2} :start-deps [] :start-fn (fn [config] (future 22)) :stop-fn (fn [obj-state])})
    (core/create! *ctx {:id :web :config {:c 3} :start-deps [] :start-fn (fn [config] (deliver p 33)) :stop-fn (fn [obj-state])})
    (core/start-all *ctx)
    (testing "get state value"
      (match (sut/state-value *ctx :db) 11)
      (match (sut/state-value @*ctx :db) 11)
      (match (sut/state-value *ctx :cache) 22)
      (match (sut/state-value @*ctx :web) 33))

    (core/stop-all *ctx)))
