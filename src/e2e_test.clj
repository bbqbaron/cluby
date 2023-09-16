(ns e2e-test 
  (:require [clojure.test :refer [deftest is]]
            parse
            [print :as p]))

(deftest method-call
  (is (= "bar.foo(1)"
         (p/print-ast
          (parse/expr->node
           '(.foo bar 1))))))

(deftest function-call
  (is (= "foo(bar, 1)"
         (p/print-ast
          (parse/expr->node
           '(foo bar 1))))))

(deftest static-method
  (is (= "Foo::bar(1)"
         (p/print-ast
          (parse/expr->node
           '(Foo/bar 1))))))

(deftest property-access 
  (is (= "a.b"
         (p/print-ast
          (parse/expr->node
           '(.b a))))))

(deftest key-access
  (is (= "b[a]"
         (p/print-ast
          (parse/expr->node
           '(get b a))))))

(deftest trailing-block
  (is (= "foo.bar(1, 2, 3) { | x | x + 5 }"
         (p/print-ast
          (parse/expr->node
           '(.bar foo 1 2 3 (fn [x] (+ x 5))))))))

(deftest cond-expr
  (is (= "if test1 then test2 elsif test3 + test4 then test6.test5
else default end"
         (p/print-ast
          (parse/expr->node
           '(cond
              test1 test2
              (+ test3 test4) (.test5 test6)
              default))))))

(deftest define-function
  (is (= "def myfun(a, b, **c)
    some_op(a, b, c)
end"
         (p/print-ast
          (parse/expr->node
           '(defn myfun [a b ** c]
              (some_op a b c)))))))

(deftest operator 
  (is (= "1 + 2 + 3 + 4"
         (p/print-ast
          (parse/expr->node
           '(+ 1 2 3 4))))))

