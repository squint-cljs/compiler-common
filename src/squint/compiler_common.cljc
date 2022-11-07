(ns squint.compiler-common
  (:require
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])
   [clojure.string :as str]))

#?(:cljs (def Exception js/Error))

#?(:cljs (def format gstring/format))

(defmulti emit (fn [expr _env] (type expr)))

(defmulti emit-special (fn [disp _env & _args] disp))

(defmethod emit-special 'js* [_ env [_js* template & substitutions]]
  (reduce (fn [template substitution]
            (str/replace-first template "~{}" (emit substitution env)))
          template
          substitutions))

(defn emit-wrap [s env]
  (if (= :return (:context env))
    (format "return %s;" s)
    s))

(defn expr-env [env]
  (assoc env :context :expr :top-level false))

(defmethod emit-special 'throw [_ env [_ expr]]
  (str "throw " (emit expr (expr-env env))))

(def statement-separator ";\n")

(def ^:dynamic *aliases* (atom {}))
(def ^:dynamic *async* false)
(def ^:dynamic *imported-vars* (atom {}))
(def ^:dynamic *excluded-core-vars* (atom #{}))
(def ^:dynamic *public-vars* (atom #{}))
(def ^:dynamic *recur-targets* (atom []))
(def ^:dynamic *repl* false)
(def ^:dynamic *cljs-ns* 'user)

(defn str-tail
  "Returns the last n characters of s."
  ^String [n ^String s]
  (if (< (count s) n)
    s
    (.substring s (- (count s) n))))

(defn statement [expr]
  (if (not (= statement-separator (str-tail (count statement-separator) expr)))
    (str expr statement-separator)
    expr))

(defn comma-list [coll]
  (str "(" (str/join ", " coll) ")"))

(defn munge* [expr]
  (let [munged (str (munge expr))
        keep #{"import" "await"}]
    (cond-> munged
      (and (str/ends-with? munged "$")
           (contains? keep (str expr)))
      (str/replace #"\$$" ""))))

(defmethod emit nil [_ env]
  (emit-wrap "null" env))

#?(:clj (derive #?(:clj java.lang.Integer) ::number))
#?(:clj (derive #?(:clj java.lang.Long) ::number))
#?(:cljs (derive js/Number ::number))

(defn emit-repl [s env]
  (if (and *repl*
           (:top-level env))
    (str "\nglobalThis._repl = " s)
    s))

(defn escape-jsx [expr env]
  (if (:jsx env)
    (format "{%s}" expr)
    expr))

(defmethod emit ::number [expr env]
  (-> (str expr)
      (emit-wrap env)
      (emit-repl env)
      (escape-jsx env)))

(defmethod emit #?(:clj java.lang.String :cljs js/String) [^String expr env]
  (-> (if (and (:jsx env)
               (not (:jsx-attr env)))
        expr
        (emit-wrap (pr-str expr) env))
      (emit-repl env)))

#?(:clj (defmethod emit #?(:clj java.util.regex.Pattern) [expr _env]
          (str \/ expr \/)))

(defmethod emit :default [expr env]
  ;; RegExp case moved here:
  ;; References to the global RegExp object prevents optimization of regular expressions.
  (emit-wrap (str expr) env))

(def prefix-unary-operators '#{!})

(def suffix-unary-operators '#{++ --})

(def infix-operators #{"+" "+=" "-" "-=" "/" "*" "%" "=" "==" "===" "<" ">" "<=" ">=" "!="
                       "<<" ">>" "<<<" ">>>" "!==" "&" "|" "&&" "||" "not=" "instanceof"})

(def chainable-infix-operators #{"+" "-" "*" "/" "&" "|" "&&" "||"})

(defn infix-operator? [expr]
  (contains? infix-operators (name expr)))

(defn prefix-unary? [expr]
  (contains? prefix-unary-operators expr))

(defn suffix-unary? [expr]
  (contains? suffix-unary-operators expr))

(defn emit-args [env args]
  (let [env (assoc env :context :expr :top-level false)]
    (map #(emit % env) args)))

(defn emit-infix [_type enc-env [operator & args]]
  (let [env (assoc enc-env :context :expr :top-level false)
        acount (count args)]
    (if (and (not (chainable-infix-operators (name operator))) (> acount 2))
      (emit (list 'cljs.core/and
                  (list operator (first args) (second args))
                  (list* operator (rest args))))
      (-> (if (and (= '- operator)
                   (= 1 acount))
            (str "-" (emit (first args) env))
            (-> (let [substitutions {'= "===" == "===" '!= "!=="
                                     'not= "!=="
                                     '+ "+"}]
                  (str "(" (str/join (str " " (or (substitutions operator) operator) " ")
                                     (emit-args env args)) ")"))
                (emit-wrap enc-env)))
          (emit-repl enc-env)))))

(def core-vars (atom #{}))

(def ^:dynamic *core-package* "squint-cljs/core.js")

(defn maybe-core-var [sym]
  (let [m (munge sym)]
    (when (and (contains? @core-vars m)
               (not (contains? @*excluded-core-vars* m)))
      (swap! *imported-vars* update *core-package* (fnil conj #{}) m)
      m)))

(defmethod emit #?(:clj clojure.lang.Symbol :cljs Symbol) [expr env]
  (if (:quote env)
    (emit-wrap (escape-jsx (emit (list 'cljs.core/symbol
                                       (str expr))
                                 (dissoc env :quote))env)
               env)
    (if (and (simple-symbol? expr)
             (str/includes? (str expr) "."))
      (let [[fname path] (str/split (str expr) #"\." 2)
            fname (symbol fname)]
        (escape-jsx (str (emit fname (dissoc (expr-env env) :jsx))
                         "." path) env))
      (let [munged-name (fn [expr] (munge* (name expr)))
            expr (if-let [sym-ns (namespace expr)]
                   (let [sn (symbol (name expr))]
                     (or (when (or (= "cljs.core" sym-ns)
                                   (= "clojure.core" sym-ns))
                           (some-> (maybe-core-var sn) munge))
                         (when (= "js" sym-ns)
                           (munge* (name expr)))
                         (when-let [resolved-ns (get @*aliases* (symbol sym-ns))]
                           (swap! *imported-vars* update resolved-ns (fnil conj #{}) (munged-name sn))
                           (str sym-ns "_"  (munged-name sn)))
                         (if *repl*
                           (str "globalThis." (munge *cljs-ns*) ".aliases." (namespace expr) "." (name expr))
                           expr)))
                   (if-let [renamed (get (:var->ident env) expr)]
                     (munge* (str renamed))
                     (or
                      (some-> (maybe-core-var expr) munge)
                      (let [m (munged-name expr)]
                        (str (when *repl*
                               (str (munge *cljs-ns*) ".")) m)))))]
        (-> (emit-wrap (escape-jsx (str expr) env)
                       env)
            (emit-repl env))))))

(defn wrap-await [s]
  (format "(%s)" (str "await " s)))

(defn wrap-iife [s]
  (cond-> (format "(%sfunction () {\n %s\n})()" (if *async* "async " "") s)
    *async* (wrap-await)))

(defn emit-do [env exprs]
  (let [bl (butlast exprs)
        l (last exprs)
        ctx (:context env)
        statement-env (assoc env :context :statement)
        iife? (and (seq bl) (= :expr ctx))
        s (cond-> (str (str/join "" (map #(statement (emit % statement-env)) bl))
                       (emit l (assoc env :context
                                      (if iife? :return
                                          ctx))))
            iife?
            (wrap-iife))]
    s))

(defmethod emit-special 'do [_type env [_ & exprs]]
  (emit-do env exprs))

(defn emit-let [enc-env bindings body is-loop]
  (let [context (:context enc-env)
        env (assoc enc-env :context :expr)
        partitioned (partition 2 bindings)
        iife? (or (= :expr context)
                  (and *repl* (:top-level env)))
        upper-var->ident (:var->ident enc-env)
        [bindings var->ident]
        (let [env (dissoc env :top-level)]
          (reduce (fn [[acc var->ident] [var-name rhs]]
                    (let [vm (meta var-name)
                          rename? (not (:squint.compiler/no-rename vm))
                          renamed (if rename? (munge (gensym var-name))
                                      var-name)
                          lhs (str renamed)
                          rhs (emit rhs (assoc env :var->ident var->ident))
                          expr (format "let %s = %s;\n" lhs rhs)
                          var->ident (assoc var->ident var-name renamed)]
                      [(str acc expr) var->ident]))
                  ["" upper-var->ident]
                  partitioned))
        enc-env (assoc enc-env :var->ident var->ident :top-level false)]
    (-> (cond->> (str
                  bindings
                  (when is-loop
                    (str "while(true){\n"))
                  ;; TODO: move this to env arg?
                  (binding [*recur-targets*
                            (if is-loop (map var->ident (map first partitioned))
                                *recur-targets*)]
                    (emit-do (if iife?
                               (assoc enc-env :context :return)
                               enc-env) body))
                  (when is-loop
                    ;; TODO: not sure why I had to insert the ; here, but else
                    ;; (loop [x 1] (+ 1 2 x)) breaks
                    (str ";break;\n}\n")))
          iife?
          (wrap-iife))
        (emit-repl env))))

(defmethod emit-special 'let* [_type enc-env [_let bindings & body]]
  (emit-let enc-env bindings body false))
