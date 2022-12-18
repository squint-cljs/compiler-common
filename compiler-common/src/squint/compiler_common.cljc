(ns squint.compiler-common
  (:refer-clojure :exclude [*target*])
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
(def ^:dynamic *target* :squint)

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

(defmethod emit #?(:clj java.lang.Boolean :cljs js/Boolean) [^String expr env]
  (-> (if (:jsx-attr env)
        (escape-jsx expr env)
        (str expr))
      (emit-wrap env)
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

(defmethod emit-special 'loop* [_ env [_ bindings & body]]
  (emit-let env bindings body true))

(defmethod emit-special 'case* [_ env [_ v tests thens default]]
  (let [expr? (= :expr (:context env))
        gs (gensym "caseval__")
        eenv (expr-env env)]
    (cond-> (str
             (when expr?
               (str "var " gs ";\n"))
             (str "switch (" (emit v eenv) ") {")
             (str/join (map (fn [test then]
                              (str/join
                               (map (fn [test]
                                      (str (str "case " (emit test eenv) ":\n")
                                           (if expr?
                                             (str gs " = " then)
                                             (emit then env))
                                           "\nbreak;\n"))
                                    test)))
                            tests thens))
             (when default
               (str "default:\n"
                    (if expr?
                      (str gs " = " (emit default eenv))
                      (emit default env))))
             (when expr?
               (str "return " gs ";"))
             "}")
      expr? (wrap-iife))))

(defmethod emit-special 'recur [_ env [_ & exprs]]
  (let [bindings *recur-targets*
        temps (repeatedly (count exprs) gensym)
        eenv (expr-env env)]
    (when-let [cb (:recur-callback env)]
      (cb bindings))
    (str
     (str/join ""
               (map (fn [temp expr]
                      (statement (format "let %s = %s"
                                         temp (emit expr eenv))))
                    temps exprs)
               )
     (str/join ""
               (map (fn [binding temp]
                      (statement (format "%s = %s"
                                         binding temp)))
                    bindings temps)
               )
     "continue;\n")))

(defn emit-repl-var [s _name env]
  (str s
       (when (and *repl* (:top-level env))
         "globalThis._repl = null;\n")))

(defn no-top-level [env]
  (dissoc env :top-level))

(defn emit-var [[name expr] env]
  (-> (let [env (no-top-level env)]
        (str (if *repl*
               (str "globalThis."
                    (when *cljs-ns*
                      (str (munge *cljs-ns*) ".") #_"var ")
                    (munge name))
               (str "var " (munge name))) " = "
             (emit expr (expr-env env)) "\n"
             (when *repl*
               (str "var " (munge name) " = " "globalThis."
                    (when *cljs-ns*
                      (str (munge *cljs-ns*) "."))
                    (munge name)
                    "\n;"))))
      (emit-repl-var name env)))

(defmethod emit-special 'def [_type env [_const & more]]
  (let [name (first more)]
    (swap! *public-vars* conj (munge* name))
    (emit-var more env)))

(defn js-await [env more]
  (-> (emit-wrap (wrap-await (emit more (expr-env env))) env)
      (emit-repl env)))

(defmethod emit-special 'js/await [_ env [_await more]]
  (js-await env more))

(defmethod emit-special 'js-await [_ env [_await more]]
  (js-await env more))

#_(defn wrap-iife [s]
    (cond-> (format "(%sfunction () {\n %s\n})()" (if *async* "async " "") s)
      *async* (wrap-await)))

(defn resolve-ns [alias]
  (if (= :squint *target*)
    (case alias
      (squint.string clojure.string) "squint-cljs/string.js"
      alias)
    alias))

(defn process-require-clause [[libname & {:keys [refer as]}]]
  (let [libname (resolve-ns libname)
        [libname suffix] (str/split (if (string? libname) libname (str libname)) #"\$" 2)
        [p & _props] (when suffix
                       (str/split suffix #"\."))
        as (when as (munge as))]
    (str
     (when-not *repl*
       (when (and as (= "default" p))
         (statement (format "import %s from '%s'" as libname))))
     (when (and (not as) (not p) (not refer))
       ;; import presumably for side effects
       (statement (format "import '%s'" libname)))
     (when as
       (swap! *imported-vars* update libname (fnil identity #{}))
       (when *repl*
         (if (str/ends-with? libname "$default")
           (statement (format "import %s from '%s'" as (str/replace libname "$default" "")))
           (statement (format "import * as %s from '%s'" as libname)))))
     (when refer
       (statement (format "import { %s } from '%s'"  (str/join ", " refer) libname))))))

(defmethod emit-special 'ns [_type _env [_ns name & clauses]]
  (set! *cljs-ns* name)
  (reset! *aliases*
          (->> clauses
               (some
                (fn [[k & exprs]]
                  (when (= :require k) exprs)))
               (reduce
                (fn [aliases [full as alias]]
                  (let [full (resolve-ns full)]
                    (case as
                      (:as :as-alias)
                      (assoc aliases (munge alias) full)
                      aliases)))
                {:current name})))
  (str
   (reduce (fn [acc [k & exprs]]
             (cond
               (= :require k)
               (str acc (str/join "" (map process-require-clause exprs)))
               (= :refer-clojure k)
               (let [{:keys [exclude]} exprs]
                 (swap! *excluded-core-vars* into exclude)
                 acc)
               :else acc))
           ""
           clauses)
   (when *repl*
     (let [mname (munge name)
           split-name (str/split (str mname) #"\.")
           ensure-obj (-> (reduce (fn [{:keys [js nk]} k]
                                    (let [nk (str (when nk
                                                    (str nk ".")) k)]
                                      {:js (str js "globalThis." nk " = {};\n")
                                       :nk nk}))
                                  {}
                                  split-name)
                          :js)
           ns-obj (str "globalThis." mname)]
       (str
        ensure-obj
        ns-obj " = {aliases: {}};\n"
        (reduce-kv (fn [acc k _v]
                     (if (symbol? k)
                       (str acc
                            ns-obj ".aliases." k " = " k ";\n")
                       acc))
                   ""
                   @*aliases*))))))

(defmethod emit-special 'require [_ _env [_ & clauses]]
  (let [clauses (map second clauses)]
    (reset! *aliases*
            (->> clauses
                 (reduce
                  (fn [aliases [full as alias]]
                    (let [full (resolve-ns full)]
                      (case as
                        (:as :as-alias)
                        (assoc aliases alias full)
                        aliases)))
                  {:current name})))
    (str (str/join "" (map process-require-clause clauses))
         (when *repl*
           (let [mname (munge *cljs-ns*)
                 split-name (str/split (str mname) #"\.")
                 ensure-obj (-> (reduce (fn [{:keys [js nk]} k]
                                          (let [nk (str (when nk
                                                          (str nk ".")) k)]
                                            {:js (str js "globalThis." nk " = {};\n")
                                             :nk nk}))
                                        {}
                                        split-name)
                                :js)
                 ns-obj (str "globalThis." mname)]
             (str
              ensure-obj
              ns-obj " = {aliases: {}};\n"
              (reduce-kv (fn [acc k _v]
                           (if (symbol? k)
                             (str acc
                                  ns-obj ".aliases." k " = " k ";\n")
                             acc))
                         ""
                         @*aliases*)))))))

(defmethod emit-special 'str [_type env [_str & args]]
  (apply clojure.core/str (interpose " + " (emit-args env args))))

(defn emit-method [env obj method args]
  (let [eenv (expr-env env)]
    (emit-wrap (str (emit obj eenv) "."
                    (str method)
                    (comma-list (emit-args env args)))
               env)))

(defn emit-aget [env var idxs]
  (emit-wrap (apply str
                    (emit var (expr-env env))
                    (interleave (repeat "[") (emit-args env idxs) (repeat "]")))
             env))

(defmethod emit-special '. [_type env [_period obj method & args]]
  (let [[method args] (if (seq? method)
                        [(first method) (rest method)]
                        [method args])
        method-str (str method)]
    (-> (if (str/starts-with? method-str "-")
          (emit-aget env obj [(subs method-str 1)])
          (emit-method env obj (symbol method-str) args))
        (emit-repl env))))

(defmethod emit-special 'aget [_type env [_aget var & idxs]]
  (emit-aget env var idxs))

;; TODO: this should not be reachable in user space
(defmethod emit-special 'return [_type env [_return expr]]
  (statement (str "return " (emit (assoc env :context :expr) env))))

#_(defmethod emit-special 'delete [type [return expr]]
    (str "delete " (emit expr)))

(defmethod emit-special 'set! [_type env [_set! var val & more]]
  (assert (or (nil? more) (even? (count more))))
  (let [eenv (expr-env env)]
    (emit-wrap (str (emit var eenv) " = " (emit val eenv) statement-separator
                    #_(when more (str (emit (cons 'set! more) env))))
               env)))

(defmethod emit-special 'new [_type env [_new class & args]]
  (emit-wrap (str "new " (emit class (expr-env env)) (comma-list (emit-args env args))) env))

(defmethod emit-special 'dec [_type env [_ var]]
  (emit-wrap (str "(" (emit var (assoc env :context :expr)) " - " 1 ")") env))

(defmethod emit-special 'inc [_type env [_ var]]
  (emit-wrap (str "(" (emit var (assoc env :context :expr)) " + " 1 ")") env))

#_(defmethod emit-special 'defined? [_type env [_ var]]
    (str "typeof " (emit var env) " !== \"undefined\" && " (emit var env) " !== null"))

#_(defmethod emit-special '? [_type env [_ test then else]]
    (str (emit test env) " ? " (emit then env) " : " (emit else env)))

(defmethod emit-special 'and [_type env [_ & more]]
  (emit-wrap (apply str (interpose " && " (emit-args env more))) env))

(defmethod emit-special 'or [_type env [_ & more]]
  (emit-wrap (apply str (interpose " || " (emit-args env more))) env))

(defmethod emit-special 'while [_type env [_while test & body]]
  (str "while (" (emit test) ") { \n"
       (emit-do env body)
       "\n }"))

(defn ->sig [env sig]
  (reduce (fn [[env sig seen] param]
            (if (contains? seen param)
              (let [new-param (gensym param)
                    env (update env :var->ident assoc param new-param)
                    sig (conj sig new-param)
                    seen (conj seen param)]
                [env sig seen])
              [(update env :var->ident assoc param param)
               (conj sig param)
               (conj seen param)]))
          [env [] #{}]
          sig))

(defn emit-function [env _name sig body & [elide-function?]]
  ;; (assert (or (symbol? name) (nil? name)))
  (assert (vector? sig))
  (let [[env sig] (->sig env sig)]
    (binding [*recur-targets* sig]
      (let [recur? (volatile! nil)
            env (assoc env :recur-callback
                       (fn [coll]
                         (when (identical? sig coll)
                           (vreset! recur? true))))
            body (emit-do (assoc env :context :return) body)
            body (if @recur?
                   (format "while(true){
%s
break;}" body)
                   body)]
        (str (when-not elide-function?
               (str (when *async*
                      "async ") "function "))
             (comma-list (map (fn [sym]
                                (let [munged (munge sym)]
                                  (if (:... (meta sym))
                                    (str "..." munged)
                                    munged))) sig)) " {\n"
             (when (:type env)
               (str "var self__ = this;"))
             body "\n}")))))

(defn emit-function* [env expr]
  (let [name (when (symbol? (first expr)) (first expr))
        expr (if name (rest expr) expr)
        expr (if (seq? (first expr))
               ;; TODO: multi-arity:
               (first expr)
               expr)]
    (-> (if name
          (let [signature (first expr)
                body (rest expr)]
            (str (when *async*
                   "async ") "function " name " "
                 (emit-function env name signature body true)))
          (let [signature (first expr)
                body (rest expr)]
            (str (emit-function env nil signature body))))
        (emit-wrap env))))

(defmethod emit-special 'fn* [_type env [_fn & sigs :as expr]]
  (let [async? (:async (meta expr))]
    (binding [*async* async?]
      (emit-function* env sigs))))

(defmethod emit-special 'try [_type env [_try & body :as expression]]
  (let [try-body (remove #(contains? #{'catch 'finally} (and (seq? %)
                                                             (first %)))
                         body)
        catch-clause (filter #(= 'catch (and (seq? %)
                                             (first %)))
                             body)
        finally-clause (filter #(= 'finally (and (seq? %)
                                                 (first %)))
                               body)
        non-statement? (not= :statement (:context env))
        outer-env env
        env (if non-statement?
              (assoc env :context :return)
              env)]
    (cond
      (and (empty? catch-clause)
           (empty? finally-clause))
      (throw (new Exception (str "Must supply a catch or finally clause (or both) in a try statement! " expression)))

      (> (count catch-clause) 1)
      (throw (new Exception (str "Multiple catch clauses in a try statement are not currently supported! " expression)))

      (> (count finally-clause) 1)
      (throw (new Exception (str "Cannot supply more than one finally clause in a try statement! " expression)))

      :else
      (-> (cond-> (str "try{\n"
                       (emit-do env try-body)
                       "}\n"
                       (when-let [[_ _exception binding & catch-body] (first catch-clause)]
                         ;; TODO: only bind when exception type matches
                         (str "catch(" (emit binding (expr-env env)) "){\n"
                              (emit-do env catch-body)
                              "}\n"))
                       (when-let [[_ & finally-body] (first finally-clause)]
                         (str "finally{\n"
                              (emit-do (assoc env :context :statement) finally-body)
                              "}\n")))
            (not= :statement (:context env))
            (wrap-iife))
          (emit-wrap outer-env)))))

#_(defmethod emit-special 'funcall [_type env [fname & args :as _expr]]
  (-> (emit-wrap (str
                  (emit fname (expr-env env))
                  ;; this is needed when calling keywords, symbols, etc. We could
                  ;; optimize this later by inferring that we're not directly
                  ;; calling a `function`.
                  #_(when-not interop? ".call")
                  (comma-list (emit-args env
                                         args #_(if interop? args
                                                    (cons nil args)))))
                 env)
      (emit-repl env)))

(defmethod emit-special 'funcall [_type env [fname & args :as _expr]]
  (let [ns (when (symbol? fname) (namespace fname))
        fname (if ns (symbol (munge ns) (name fname))
                  fname)
        cherry? (= :cherry *target*)
        cherry+interop? (and
                         cherry?
                         (= "js" ns))]
    (-> (emit-wrap (str
                    (emit fname (expr-env env))
                    ;; this is needed when calling keywords, symbols, etc. We could
                    ;; optimize this later by inferring that we're not directly
                    ;; calling a `function`.
                    (when (and cherry? (not cherry+interop?)) ".call")
                    (comma-list (emit-args env
                                           (if cherry?
                                             (if (not cherry+interop?)
                                               (cons nil args)
                                               args)
                                             args
                                             ))))
                   env)
        (emit-repl env))))
