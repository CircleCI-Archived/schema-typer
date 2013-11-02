(ns circle.schema-type-def
  (:require [circle.schema-typer :as st]))

;; data used for testing
(st/def-schema-type User {:login String
                          :login-count Number})
