(ns squint.compiler-common
  (:require
   [clojure.string :as str]
   ;; [com.reasonr.string :as rstr]
   [edamame.core :as e]
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])))

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
