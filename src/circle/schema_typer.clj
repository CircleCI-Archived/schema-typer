(ns circle.schema-typer
  "Creates core.typed types from prismatic/schema definitions. Inspired by https://gist.github.com/c-spencer/6569571"
  (:require [clojure.core.typed :as t]))

(defn convert-dispatch [schema]
  (cond
   (class? schema) schema
   :else (class schema)))

(defmulti convert "" #'convert-dispatch )

(defmethod convert Number [s]
  'java.lang.Number)

(defmethod convert String [s]
  'java.lang.String)

(defmethod convert clojure.lang.Symbol [s]
  s)

(defn convert-hmap
  "Returns a core.typed HMap. All keys of the map must be keywords"
  [s]
  (assert (map? s))
  (list 'HMap :mandatory (->>
                          (for [[k v] s]
                            (do
                              (assert (keyword? k))
                              [k (convert v)]))
                          (into {}))))

(defn class->name [c]
  (-> c .getName symbol))

(defn convert-map [s]
  (assert (map? s))
  (assert (= 1 (count s)) "convert-map only supports one kv")
  (let [kt (-> s first key)
        vt (-> s first val)]
    (assert (class kt))
    (assert (class vt))
    (list 't/Map (class->name kt) (class->name vt))))

(defn non-hmap? [s]
  (and (map? s)
       (class? (first (keys s)))))

(defmethod convert clojure.lang.IPersistentMap [s]
  (if (non-hmap? s)
    (convert-map s)
    (convert-hmap s)))

(defn schema->type
  "Takes a prismatic schema. Returns a list of symbols that can be understood as a core.typed type."
  [s]
  ;; {:post [(do (println "schema->type" s %) true) (or (symbol? %) (list? %))]}
  (convert s))

(defmacro def-schema-type
  "creates a def-alias named type-name, from schema type"
  [type-name s]
  `(def-alias ~type-name ~(schema->type ~s)))
