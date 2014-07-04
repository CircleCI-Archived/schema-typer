schema-typer
============

Creates core.typed types from prismatic/schema definitions. Inspired by https://gist.github.com/c-spencer/6569571

```clojure
[org.ambrosebs/schema-typer "0.2.4"]
```
Requires schema > 0.2.0


Motivation
==========
Q: Why the hell build this?

A: core.typed is good at compile-time validation. prismatic/schema is
good at run-time validation. schema-typer avoids duplication between your
core.type definition, and your schema definition. Having types for
schemas allows you to prove at compile-time that validation is
happening, at runtime.

Usage
=====
```clojure
(:require [schema.core :as s]
          [circle.schema-typer :refer (def-schema-alias def-validator)])

(def user-schema {:login String}) ;; define a normal prismatic schema

(def-schema-alias User user-schema)
;; defines (def-alias User (HMap :mandatory {:login String}))

(def-validator validate-user User user-schema)
;; defines
;; (t/ann validate-user [Any -> User])
;; (defn validate-user [x]
;;   (s/validate user-schema x))
```

Limitations
===========

It's pretty janky atm. There are a few critical ^:no-checks, because
core.typed doesn't yet 'believe' that schema is validating
properly. Currently, def-validators are ^:no-checked, but they do call (schema.core/validate schema) so you get proper checking at runtime.

The mapping from schema -> core.typed is currently extremely incomplete, but it should be obvious how to extend. Patches welcome.

License
=======
EPL 1.0, the same as Clojure
