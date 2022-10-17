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

(defmethod emit ::number [expr env]
  (-> (str expr)
      (emit-wrap env)
      (emit-repl env)))

(defmethod emit #?(:clj java.lang.String :cljs js/String) [^String expr env]
  (-> (if (and (:jsx env)
               (not (:jsx-attr env)))
        expr
        (emit-wrap (pr-str expr) env))
      (emit-repl env)))

#?(:clj (defmethod emit #?(:clj java.util.regex.Pattern) [expr _env]
          (str \/ expr \/)))

(defn escape-jsx [env expr]
  (if (:jsx env)
    (format "{%s}" expr)
    expr))

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
