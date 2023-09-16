# cluby

Clojure _syntax_ to Ruby transpiler.

Largely Ruby semantics, data structures, and stdlib.

Transpiles a _very_ limited and probably confusing set of Clojure stdlib behaviors to Ruby:
* variadic operators
* `when` as `if`

Huge swathes of both languages totally unaddressed. Covered enough of Ruby to use for scripting a game that didn't use classes, lambdas, or many other Ruby features.
