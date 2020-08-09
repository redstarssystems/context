(ns org.rssys.context.helpers
  "Helpers for Context library"
  (:require [org.rssys.context.core :as core])
  (:import (clojure.lang Atom Keyword)))

(defn- atom? [v] (instance? Atom v))

(defn- component-value
  "Extract any value from component structure."
  [ctx ^Keyword id-kwd ^Keyword val-kwd]
  (let [comp-value (if (atom? ctx)
                     (core/get-component ctx id-kwd)
                     (core/get-component-value ctx id-kwd))]
    (get comp-value val-kwd)))

(defn config-value
  "Get :config value from component using its id"
  [ctx ^Keyword id-kwd]
  (component-value ctx id-kwd :config))

(defn state-value
  "Get :state-obj value from component using its id.
   If returned value is clojure.lang.IDeref then deref it otherwise return as is."
  [ctx ^Keyword id-kwd]
  (let [result (component-value ctx id-kwd :state-obj)]
    (if (instance? clojure.lang.IDeref result)
      (deref result)
      result)))




