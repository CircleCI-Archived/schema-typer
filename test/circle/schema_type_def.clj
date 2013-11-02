(ns circle.schema-type-def
  (:require [clojure.core.typed :as t]
            [circle.schema-typer :as st]))

;; data used for testing
(st/def-schema-type User {:login String
                          :login-count Number})

(t/ann test-user User)
(def test-user {:login "foo"
                 :login-count 5})
