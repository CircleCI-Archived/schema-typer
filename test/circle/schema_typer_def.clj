(ns circle.schema-typer-def
  (:require [clojure.core.typed :as t]
            [schema.core :as s]
            [circle.schema-typer :as st]))

;; data used for testing
(t/ann user-schema st/Schema)
(def user-schema {:login String
                  :login-count Number})

(st/def-schema-alias User user-schema)
(st/def-validator validate-user User user-schema)

(t/ann foo [-> User])
(defn foo []
  (validate-user {:login "arohner"}))
