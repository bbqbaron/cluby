(ns print 
  (:require ast
            [clojure.string :as str]))

;; TODO nesting indentation if anyone is going to read the code

(defmulti print-ast-inner first)

(declare print-ast)

(defn wrap 
  "precedence sucks. everyone should use lisp."
  [code]
  (format "(%s)" code))

(defmethod print-ast-inner :ast/spread
  [[_ symbol]]
  (format "**%s" (name symbol)))

(defmethod print-ast-inner :ast/require 
  [[_ target]]
  (str "require " (print-ast target)))

(defmethod print-ast-inner :ast/symbol
  [[_ value]]
  (format ":%s" value))

(defmethod print-ast-inner :ast/call
  [[_ referent {:keys [method trailer static?]} & args]]
  (let [function-expr (str/join (if static? "::" ".")
                                (map 
                                 print-ast 
                                 (filter some? 
                                         [referent
                                          method])))]
    (format "%s(%s)%s"
            function-expr
            (str/join ", "
                      (map print-ast args))
            (if trailer
              (format " %s" (print-ast trailer))
              ""))))

(defmethod print-ast-inner :ast/bool-lit
  [[_ b]] (str b))

;; XXX `symbol` is anything we just print as itself.
;; this is almost certainly too crude.
(defmethod print-ast-inner :ast/name [[_ sym-name]] sym-name)

(defmethod print-ast-inner :ast/map-lit
  [[_ entries]]
  (format "{%s}"
          (str/join ", "
                    (for [[k v] entries]
                      (format "%s => %s"
                              (print-ast k)
                              (print-ast v))))))

(defmethod print-ast-inner :ast/setval
 [[_ on-what set-key set-value]]
 (format "%s[%s] = %s" (print-ast on-what) (print-ast set-key) (print-ast set-value)))

(defmethod print-ast-inner :ast/assign-var
 [[_ var-name value]]
 (format "%s = %s"
         (print-ast var-name)
         (print-ast value)))

(defmethod print-ast-inner :ast/while
  [[_ condition & body]]
  (format "while %s do 
    %s
end"
          (print-ast condition)
          (str/join "    \n"
                    (map print-ast body))))

(defmethod print-ast-inner :ast/def
  [[_ fn-name fn-args & body]]
  (format "def %s%s
    %s
end"
          (print-ast fn-name)
          (format "(%s)" (str/join ", " (map print-ast fn-args)))
          (str/join "    \n"
                    (map print-ast-inner body))))

(defmethod print-ast-inner :ast/array-lit
 [[_ items]]
 (format "[%s]" (str/join ", " (map print-ast items))))

(defmethod print-ast-inner :ast/key-access
 [[_ object key]]
 (format "%s[%s]"
         (print-ast object)
         (print-ast key)))

(defmethod print-ast-inner :ast/num-lit
 [[_ num]]
  num)

(defmethod print-ast-inner :ast/str-lit
 [[_ value]]
  (pr-str value))

(defmethod print-ast-inner :ast/operator
 [[_ op & args]]
 (str/join (format " %s " (print-ast op))
           (map print-ast args)))

(defmethod print-ast-inner :ast/range
 [[_ lo hi]]
  ;; TODO systematic designation of expressions that can be wrapped.
  ;; until we care about prettiness, we can't wrap anything wrappable, but some node elements
  ;; aren't expressions. you can't parenthesize function bindings.
 (wrap (format "%s..%s" (print-ast lo) (print-ast hi))))

(defmethod print-ast-inner :ast/member-access
  [[_ on-what member]]
  (format "%s.%s"
          (print-ast on-what)
          (print-ast member)))

(defmethod print-ast-inner :ast/case
  [[_ subject & clauses]]
  (format "case %s
    %s
end"
          (print-ast subject)
          (str/join "    \n"
                    (for [[a b] clauses]
                      (cond
                        (not b)
                        (format "else %s" (str/join "\n" (map print-ast a)))
                        (= a 'else)
                        (format "else %s" (str/join "\n" (map print-ast b)))
                        :else
                        (format "when %s then\n%s" (str/join "\n" (map print-ast a)) (str/join "\n" (map print-ast b))))))))

(defmethod print-ast-inner :ast/block
 [[_ bindings & body]]
 (format "{ | %s | %s }"
         (str/join ", " (map print-ast bindings))
         (str/join "\n" 
                   (map print-ast body))))

(defmethod print-ast-inner :ast/if
  [[_ cond1 body1 & more]]
  (format "if %s then %s %s end"
          (print-ast cond1)
          (str/join "\n"
                    (map print-ast body1))
          (str/join "\n"
                    (for [[test-or-else body] more]
                      (if body
                        (format "elsif %s then %s" 
                                (str/join "\n" (map print-ast test-or-else)) 
                                (str/join "\n" (map print-ast body)))
                        (format "else %s" (str/join "\n" (map print-ast test-or-else))))))))

(defmethod print-ast-inner :ast/tern
 [[_ test if-true if-false]]
 (format "%s ? %s : %s"
         (print-ast test)
         (print-ast if-true)
         (print-ast if-false)))

(defn print-ast [ast]
  (print-ast-inner ast))

(comment
  (print-ast 
   [:ast/if
    [:ast/name 'q]
    [:ast/name 'v]])

  (print-ast [:ast/str-lit "hi \"there\""])
  (print-ast [:ast/call
              [:ast/name "x"]
              {}
              [:ast/map-lit [[:hi [:ast/name "y"]]]]
              ast/nil-lit
              [:ast/array-lit [ast/nil-lit]]])
  (print-ast
   [:ast/operator
    '+
    [:ast/num-lit 2]
    [:ast/num-lit 3]])
  (println
   (print-ast
    [:ast/def
     'a
     ['b 'c]
     [:ast/name 'd]]))
  
  (print-ast
   [:ast/call [:ast/name 'b]
    {:static? false, :method [:ast/name '.a], :trailer ["{ |x|x}"]}
    [:ast/spread 'c]]
   )
  )