(ns parse
  (:require ast
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(declare expr->node)

(defn thread-first
  "Convert '.'-joined operations to individual expressions."
  [ops]
  (seq
   (reduce
    (fn add-operation [acc op]
      (seq [op acc]))
    (mapcat
     (comp
      (partial mapv symbol)
      (fn [s]
        (let [[lead & follow] (str/split s #"\.")]
          (->>
           (cons lead
                 (mapv (partial str ".") follow))
           (remove str/blank?))))
      name)
     ops))))

(defn parse-bindings
  "Turn symbol sequences into bindings with modifiers 
   like '**'"
  [bindings]
  (loop [[b & more] bindings
         out []]
    (if b
      (case b
        ** (recur
            (rest more)
            (conj out
                  [:ast/spread (first more)]))
        (recur more
               (conj out (expr->node b))))
      out)))

(defn fn-call [somefn args]
  (let [method? (and
                 ((some-fn symbol? string?) somefn)
                 (str/starts-with? (name somefn) "."))
        [_ klazz static-method] (re-find #"(.+)/(.+)" (str somefn))
        referent (cond
                   method?
                   (expr->node (first args))
                   static-method
                   [:ast/name (str klazz)]
                   :else
                   (expr->node somefn))
        fn-args (cond-> args method? rest)
        trailer (let [larg (last fn-args)]
                  (when (and (sequential? larg) (= 'fn (first larg))) larg))
        regular-args (if trailer (butlast fn-args) fn-args)]
    (if (and method?
             (not (or (seq regular-args) trailer)))
      [:ast/member-access
       referent
       [:ast/name (second
                   (re-find #"\.(.+)" (str somefn)))]]
      (into
       [:ast/call
        referent
        {:static? (some? static-method)
       ;; TODO method names probably don't have exactly the same rules as symbols
         :method (if static-method
                   [:ast/name static-method]
                   (when method?
                     [:ast/name (second
                                 (re-find #"\.(.+)" (str somefn)))]))
         :trailer (some-> trailer expr->node)}]
       (loop [[arg & args] regular-args
              out []]
         (if arg
           (case arg
             ** (recur
                 (rest args)
                 (conj out
                       [:ast/spread (first args)]))
             (recur args
                    (conj out (expr->node arg))))
           out))))))

(defn expr->nodes
  "TODO just make this the default recursion point
   with a slightly annoying refactor.
   
   Note that Ruby does not have a `do` a la lisp AFAIK,
   so returning multiple top-level nodes may not always be valid."
  [expr]
  (match expr
    (('let bindings & body) :seq) (concat
                                   (for [[n v] (partition-all 2 bindings)]
                                     [:ast/assign-var
                                      (expr->node n)
                                      (expr->node v)])
                                   (mapcat expr->nodes body))
    (('do & body) :seq) (mapcat expr->nodes body)
    :else [(expr->node expr)]))

(defn expr->node [clj-expr]
  (match clj-expr
    (_ :guard nil?) ast/nil-lit
    (m :guard map?)
    [:ast/map-lit (partition 2 (mapcat (partial mapv expr->node) m))]
    (vc :guard vector?)
    [:ast/array-lit (mapv expr->node vc)]
    (((somefn & args) :seq) :guard sequential?)
    (case somefn
      eq (into
          [:ast/operator
           [:ast/name "=="]]
          (mapv expr->node args))
      ;; TODO modules
      declare nil
      import nil
      break ast/break-lit
      assoc
      (into [:ast/setval] (mapv expr->node args))
      (def set! =)
      (into [:ast/assign-var] (mapv expr->node args))
      defn
      (let [[fn-name bindings & body] args]
        (into [:ast/def
               [:ast/name fn-name]
               (parse-bindings bindings)]
              (mapcat expr->nodes body)))
      ->
      (expr->node (thread-first args))
      get
      (into [:ast/key-access] (mapv expr->node args))
      ;; TODO others, also precedence
      (- * + % != || >= <= > < / << && -= += *= ||= ==)
      (if (= 1 (count args))
        ;; NB we're not statically validating that they're not using an operator
        ;; like a number sign, eg '-=5'
        ;; NB using java platform numbers is probably wrong here; 
        ;; should embed more deeply using strings.
        [:ast/num-lit (* (case (first args) - -1 + 1) (second args))]
        (into [:ast/operator]
              (mapv expr->node clj-expr)))
      ..
      (into [:ast/range] (mapv expr->node args))
      .-
      (let [[what access] args]
        (into [:ast/member-access
               (expr->node what)
               [:ast/name (second (re-find #"\.?(.+)" (name access)))]]))
      ;; TODO require a fallthrough case or blow up
      case
      (let [[subject & clauses] args]
        (into [:ast/case (expr->node subject)]
              (mapv
               (fn [[test result]]
                 [(expr->nodes test)
                  (some-> result expr->nodes)])
               (partition-all 2 clauses))))
      fn
      ;; TODO there are also lambdas.
      ;; IIFEs are possible in Ruby, but only via lambda, i think (?).
      ;; `fn` may be too crude here, sadly, or worse we have to guess which it is
      ;; by where it is in the AST :|.
      (let [[bindings & body] args]
        (into [:ast/block (parse-bindings bindings)]
              (mapcat expr->nodes body)))
      when
      (let [[test & body] args]
        [:ast/if (expr->node test) (mapcat expr->nodes body)])
      (cond if) 
      (let [[check if-so & pairs] args]
        (into [:ast/if (expr->node check)
               [(expr->node if-so)]]
              (partition-all 2
                             (map
                              (fn [expr]
                                (match expr
                                  (('do & exprs) :seq) (mapv expr->node exprs)
                                  _ [(expr->node expr)]))
                              pairs))))
      tern
      (into [:ast/tern] (mapv expr->node args))
      while
      (into [:ast/while] (mapv expr->node args))
      require
      (let [[_ [req-file]] args]
        [:ast/require [:ast/str-lit (format 
                                     "%s.rb"
                                     (str/replace  req-file #"\." "/"))]])
      ns
      nil
      (fn-call somefn args))
    (k :guard keyword?) [:ast/symbol (name k)]
    (s :guard string?) [:ast/str-lit s]
    (n :guard number?) [:ast/num-lit n]
    (x :guard symbol?) [:ast/name (str x)]
    (b :guard boolean?) [:ast/bool-lit b]))

(defn parse-file [clojure-exprs]
  (keep expr->node clojure-exprs))

(comment
  (expr->node
   '(while true
      (puts "Hi")
      (put "hi")))

  (expr->node
   '(A/b c d))
  
  (expr->node 
   '(-> args .state.enemy_level))
  
  (expr->node
   '(Range/new 1))
  
  (expr->node '(a))

  (expr->node
   '(case (.type v)
      :hero
      0
      :buff
      SUBCELL_WIDTH
      :partial_wall
      SUBCELL_WIDTH
      :wall
      SUBCELL_WIDTH
      (* 2 SUBCELL_WIDTH)))

  (expr->node
   '(args.state.enemy_level))
  (match '(do 1 2 3)
    (('do a b c) :seq) [a b c])
  )