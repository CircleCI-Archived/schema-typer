(ns circle.schema-typer-test
  (:import (clojure.lang Keyword))
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [schema.core :as s]
            [circle.schema-typer :as st]))

(defn is-equiv
  "Takes a value, a schema and a *quoted* type, i.e. '(HMap :mandatory ...). Asserts that v passes both schema and type"
  [v s t]
  (is (s/validate s v))
  (is (t/check-form* v t))
  (is (t/check-form* v (st/schema->type s))))

(deftest numbers-work
  (is-equiv 3 s/Num 'java.lang.Number))

(deftest any
  (is-equiv "foo" s/Any 'Any))

(deftest schema-pred
  (is-equiv :foo s/Keyword 'clojure.lang.Keyword))

(deftest hmaps-work
  (let [v {:foo 5}
        s {:foo Number}
        t '(HMap :mandatory {:foo java.lang.Number})]
    (is-equiv v s t)))

(deftest maps-work
  (let [v {:bar "foo"}
        s {Keyword String}
        t (list 'clojure.core.typed/Map 'clojure.lang.Keyword 'java.lang.String)]
    (is-equiv v s t)))

(deftest nested-hmaps-work
  (let [v {:foo {:bar "baz"}}
        s {:foo {:bar String}}
        t '(HMap :mandatory {:foo (HMap :mandatory {:bar java.lang.String})})]
    (is-equiv v s t)))

(deftest mixed-map-hmap
  (let [v {:foo {:bar "baz"}
           :not-listed "String"}
        s {:foo {:bar String}
           s/Keyword s/Any}
        t '(HMap :mandatory {:foo (HMap :mandatory {:bar java.lang.String})})]
    (is-equiv v s t)))

(deftest optional-keys
  (let [s {:foo Number
           (s/optional-key :bar) String}
        t '(HMap :mandatory {:foo java.lang.Number} :optional {:bar java.lang.String})]
    (is-equiv {:foo 3} s t)
    (is-equiv {:foo 3
               :bar "hello"} s t)))

(deftest vectors
  (let [s [{:foo Long}]
        t '(clojure.core.typed/Vec (HMap :mandatory {:foo Integer}))]
    (is-equiv [{:foo 3}] s t)))

(deftest real-use-works
  (is (t/check-ns 'circle.schema-typer-def)))
