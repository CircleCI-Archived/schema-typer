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
(def-alias CoreType (U Symbol (t/Seq (U Symbol Keyword CoreType))))

(ann schema.core/validate [Schema Any -> Any])

(defn convert-predicate-dispatch [s]
  (:pred-name s))

(defmulti convert-predicate "" #'convert-predicate-dispatch)

(defmethod convert-predicate 'keyword? [s]
  'clojure.lang.Keyword)

(ann convert-dispath [Schema -> Class])
(defn convert-dispatch [schema]
  (cond
   (= schema.core.Predicate (class schema)) :predicate
   (class? schema) schema
   :else (class schema)))

(ann ^:no-check convert [Schema -> CoreType])
(defmulti convert "" #'convert-dispatch)

(defmethod convert :predicate [s]
  (convert-predicate s))

(defmethod convert Number [s]
  'java.lang.Number)

(defmethod convert String [s]
  'java.lang.String)

(defmethod convert Keyword [s]
  'clojure.lang.Keyword)

(defmethod convert Boolean [s]
  'java.lang.Boolean)

(defmethod convert schema.core.AnythingSchema [s]
  'Any)

(defmethod convert clojure.lang.Symbol [s]
  s)

(ann hmap-grouper [(IMapEntry Any Any) -> (U (Value :mandatory)
                                             (Value :optional))])
(defn hmap-grouper
  [kv]
  (if (= schema.core.OptionalKey (class (key kv)))
    :optional
    :mandatory))

(ann convert-map [(t/Map Schema Schema) -> CoreType])
(defn convert-map [s]
  (assert (map? s))
  (assert (= 1 (count s)) "convert-map only supports one kv")
  (let [ks (-> s first key)
        vs (-> s first val)
        kt (convert ks)
        vt (convert vs)]
    (list 'clojure.core.typed/Map kt vt)))

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

(defn hmap-key?
  "True if this key is a valid pure-hmap key, i.e. Keyword or OptionalKey "
  [k]
  (or (keyword? k)
      (= schema.core.OptionalKey (class k))))

(defn pure-map? [s]
  (and (map? s)
       (every? (comp not hmap-key?) (keys s))))

(defn pure-hmap? [s]
  (and (map? s)
       (every? hmap-key? (keys s))))

(defn intersection [t1 t2]
  (list 'I t1 t2))

(defn split-map
  [s]
  "given a schema that contains HMap and t/Map keys, split and return two maps"
  (let [{hmap true
         map false} (group-by (fn [[k v]]
                                (hmap-key? k)) s)]
    {:hmap (into {} hmap)
     :map (into {} map)}))

(defmethod convert clojure.lang.IPersistentMap [s]

  ;; three kinds of maps:
  ;; 'pure' map {Keyword String} -> (t/Map Keyword String)
  ;; 'pure' hmap: {:foo String} -> (HMap :mandatory {:foo String})
  ;; 'mixed': {:foo String, Keyword String} -> (I (t/Map Keyword STring) (HMap :mandatory {:foo String}

  (assert (map? s))
  (cond
   (pure-map? s) (convert-map s)
   (pure-hmap? s) (convert-hmap s)
   :else (let [{:keys [hmap map]} (split-map s)]
           (intersection (convert-hmap hmap) (convert-map map)))))

(defmethod convert clojure.lang.IPersistentVector [s]
  (assert (= 1 (count s)))
  (list 'clojure.core.typed/Vec (convert (first s))))

(t/ann schema->type [Schema -> CoreType])
(defn schema->type
  "Takes a prismatic schema. Returns a list of symbols that can be understood as a core.typed type."
  [s]
  (convert s))

(defmacro def-schema-alias
  "creates a def-alias named type-name, from schema type"
  [type-name s]
  (let [s (eval s) ;; TODO can this be removed?
        concrete-type (schema->type s)]
    `(def-alias ~type-name ~concrete-type)))

(defmacro def-validator
  "defns a fn of type [Any -> type] that throws on validation failure. type should be a def-alias created by def-schema-alias"
  [validator-name type schema]
  `(do
     (t/ann ~(vary-meta validator-name assoc :no-check true)  [~'Any ~'-> ~type])
     (defn ~validator-name [x#]
       (s/validate ~schema x#))))
