(ns circle.schema-typer-test
  (:import (clojure.lang Keyword))
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [schema.core :as s]
            [circle.schema-typer :as st]))

(defn is-equiv
  "Takes a value, a schema and a *quoted* type, i.e. '(HMap :mandatory ...). Asserts a bunch of stuff."
  [v s t]
  (is (= t (st/schema->type s)))
  (is (s/validate s v))
  (is (t/check-form* v t)))

(deftest leaves-work
  (is (= 'java.lang.Number (st/schema->type s/Number)))
  (is-equiv 3 s/Number Number))

(deftest hmaps-work
  (let [v {:foo 5}
        s {:foo java.lang.Number}
        t '(HMap :mandatory {:foo java.lang.Number})]
    (is-equiv v s t)))

;; (deftest maps-work
;;   (let [v {:bar "foo"}
;;         s {Keyword String}
;;         t '(Map Keyword String)]
;;     (is-equiv v s t)))
