(ns circle.schema-typer
  "Creates core.typed types from prismatic/schema
   definitions. Inspired by https://gist.github.com/c-spencer/6569571"
  (:import (clojure.lang Symbol Keyword IMapEntry))
  (:require [clojure.core.typed :as t :refer (ann def-alias)]
            [schema.core :as s]))

(t/warn-on-unannotated-vars)

(ann clojure.core/val (All [x] (Fn [(clojure.lang.IMapEntry Any x) -> x])))
(ann clojure.core/group-by (All [x y] [(Fn [x -> y]) (t/Seq x) -> (t/Map y (t/Seq x))]))

;; refine these later
(def-alias Schema (U Number (t/Map Any Any) (HMap)))
(def-alias CoreType (U Symbol (t/Seq Any)))

(ann convert-dispath [Schema -> Class])
(defn convert-dispatch [schema]
  (cond
   (class? schema) schema
   :else (class schema)))

(ann ^:no-check convert [Schema -> CoreType])
(defmulti convert "" #'convert-dispatch)

(defmethod convert Number [s]
  'Number)

(defmethod convert String [s]
  'String)

(defmethod convert Keyword [s]
  'Keyword)

(defmethod convert clojure.lang.Symbol [s]
  s)

(ann hmap-grouper [(IMapEntry Any Any) -> (U (Value :mandatory)
                                             (Value :optional))])
(defn hmap-grouper
  [kv]
  (if (= schema.core.OptionalKey (class (key kv)))
    :optional
    :mandatory))

(ann ^:no-check convert-hmap [(HMap) -> CoreType])
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
    (list 'HMap
          :mandatory (convert-kvs mandatory)
          :optional (convert-kvs optional))))

(ann class->name [Class -> Symbol])
(defn class->name [^Class c]
  (-> c .getName symbol))

(ann convert-map [(t/Map Any Any) -> CoreType])
(defn convert-map [s]
  (assert (map? s))
  (assert (= 1 (count s)) "convert-map only supports one kv")
  (let [kt (-> s first key)
        vt (-> s first val)]
    (assert (= Class (class kt)))
    (assert (= Class (class vt)))
    (list 't/Map (class->name kt) (class->name vt))))

(ann non-hmap? [Schema -> Boolean])
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

(defmacro def-schema-type
  "creates a def-alias named type-name, from schema type"
  [type-name s]
  `(def-alias ~type-name ~(schema->type s)))
