(ns circle.schema-typer
  "Creates core.typed types from prismatic/schema
   definitions. Inspired by https://gist.github.com/c-spencer/6569571"
  (:import (clojure.lang Keyword IMapEntry))
  (:require [clojure.core.typed :as t]
            [schema.core :as s]))

(t/warn-on-unannotated-vars)

(t/ann clojure.core/group-by (t/All [x y] [(t/IFn [x -> y]) (t/Seq x) -> (t/Map y (t/Seq x))]))

;; refine these later
(t/defalias Schema (t/U Number (t/Map t/Any t/Any) (t/HMap)))
(t/defalias CoreType (t/U t/Sym (t/Seq t/Any)))

(t/ann schema.core/validate [Schema t/Any -> t/Any])

(t/ann convert-dispath [Schema -> Class])
(defn convert-dispatch [schema]
  (cond
   (class? schema) schema
   :else (class schema)))

(t/ann ^:no-check convert [Schema -> CoreType])
(defmulti convert "" #'convert-dispatch)

(defmethod convert Number [s]
  'java.lang.Number)

(defmethod convert String [s]
  'java.lang.String)

(defmethod convert Keyword [s]
  'clojure.lang.Keyword)

(defmethod convert clojure.lang.Symbol [s]
  s)

(t/ann hmap-grouper [(IMapEntry t/Any t/Any) -> (t/U (t/Val :mandatory)
                                                     (t/Val :optional))])
(defn hmap-grouper
  [kv]
  (if (= schema.core.OptionalKey (class (key kv)))
    :optional
    :mandatory))

(t/ann ^:no-check convert-hmap [(t/HMap) -> CoreType])
(defn convert-hmap
  "Returns a core.typed HMap."
  [s]
  (assert (map? s))
  (let [{:keys [mandatory optional]} (group-by hmap-grouper s)
        ;; strip the schema.core.OptionalKey off optionals
        optional (map (fn [[k v]]
                        [(:k k) v]) optional)
        convert-kvs (fn [kvs]
                      (->>
                       (for [[k v] kvs]
                         (do
                           [k (convert v)]))
                       (into {})))]
    (list `t/HMap
          :mandatory (convert-kvs mandatory)
          :optional (convert-kvs optional))))

(t/ann class->name [Class -> t/Sym])
(defn class->name [^Class c]
  (-> c .getName symbol))

(t/ann convert-map [(t/Map t/Any t/Any) -> CoreType])
(defn convert-map [s]
  (assert (map? s))
  (assert (= 1 (count s)) "convert-map only supports one kv")
  (let [kt (-> s first key)
        vt (-> s first val)]
    (assert (= Class (class kt)))
    (assert (= Class (class vt)))
    (list 'clojure.core.typed/Map (class->name kt) (class->name vt))))

(t/ann non-hmap? [Schema -> Boolean])
(defn non-hmap? [s]
  (and (map? s)
       (class? (first (keys s)))))

(defmethod convert clojure.lang.IPersistentMap [s]
  (assert (map? s))
  (if (non-hmap? s)
    (convert-map s)
    (convert-hmap s)))

(t/ann schema->type [Schema -> CoreType])
(defn schema->type
  "Takes a prismatic schema. Returns a list of symbols that can be understood as a core.typed type."
  [s]
  (convert s))

(defmacro def-schema-alias
  "creates a defalias named type-name, from schema type"
  [type-name s]
  (let [s (eval s)
        concrete-type (schema->type s)]
    `(t/defalias ~type-name ~concrete-type)))

(defmacro def-validator
  "defns a fn of type [Any -> type-name] that throws on validation failure. type should be a defalias created by def-schema-alias"
  [validator-name type schema]
  `(do
     (t/ann ~(vary-meta validator-name assoc :no-check true) [t/Any :-> ~type])
     (defn ~validator-name [x#]
       (s/validate ~schema x#))))
