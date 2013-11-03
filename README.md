schema-typer
============

Creates core.typed types from prismatic/schema definitions. Inspired by https://gist.github.com/c-spencer/6569571

Motivation
==========
Q: Why the hell build this?

A: core.typed is good at compile-time validation. prismatic/schema is
good at run-time validation. This avoids duplication between your
core.type definition, and your schema definition. Having proper
types for schemas allows you to prove at compile-time that validation
is properly happening, at runtime.

Usage
=====
```clojure
(:require [schema.core :as s]
          [circle.schema-typer :refer (def-schema-type def-validator])

(def user-schema {:login String}) ;; define a normal prismatic schema

(def-schema-type User user-schema)
;; now we have (def-alias User (HMap :mandatory {:login}))

(def-validator validate-user)
;; now we have
;; (t/ann validate-user [Any -> User])
;; (defn validate-user [x] ...)
```

Limitations
===========

It's pretty janky atm. There are a few critical ^:no-checks, because
core.typed doesn't yet 'believe' that schema is validating
properly. Currently, all proof that this is safe at all is in the unit
tests, 'real' checking doesn't happen when using
this. (schema.core/validate schema) is still called in def-validator,
so you get proper checking at runtime

The mapping from schema -> core.typed is currently extremely incomplete, but it should be obvious how to extend. Patches welcome.

License
=======
EPL 1.0, the same as Clojure
