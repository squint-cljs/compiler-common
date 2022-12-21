(ns squint.compiler.node
  (:require
   ["fs" :as fs]
   ["path" :as path]
   [squint.compiler :as compiler]
   [clojure.string :as str]
   [edamame.core :as e]
   [sci.core :as sci]))

(defn slurp [f]
  (fs/readFileSync f "utf-8"))

(defn spit [f s]
  (fs/writeFileSync f s "utf-8"))

(def classpath-dirs ["." "src"])

(defn resolve-file* [dir munged-macro-ns]
  (let [exts ["cljc" "cljs"]]
    (some (fn [ext]
            (let [full-path (path/resolve dir (str munged-macro-ns "." ext))]
              (when (fs/existsSync full-path)
                full-path)))
          exts)))

(defn resolve-file [macro-ns]
  (let [path (-> macro-ns str (str/replace "-" "_"))]
    (some (fn [dir]
            (resolve-file* dir path))
          classpath-dirs)))

(def ctx (sci/init {:load-fn (fn [{:keys [namespace]}]
                               (let [f (resolve-file namespace)
                                     fstr (slurp f)]
                                 {:source fstr}))}))

(defn scan-macros [s]
  (let [maybe-ns (e/parse-next (e/reader s) compiler/squint-parse-opts)]
    (when (and (seq? maybe-ns)
               (= 'ns (first maybe-ns)))
      (let [[_ns _name & clauses] maybe-ns
            require-macros (some #(when (and (seq? %)
                                             (= :require-macros (first %)))
                                    (rest %))
                                 clauses)]
        (when require-macros
          (reduce (fn [prev require-macros]
                    (.then prev
                           (fn [_]
                             (let [[macro-ns & {:keys [refer]}] require-macros
                                   macros (js/Promise.resolve
                                           (do (sci/eval-form ctx (list 'require (list 'quote macro-ns)))
                                               (let [publics (sci/eval-form ctx
                                                                            `(ns-publics '~macro-ns))
                                                     ks (keys publics)
                                                     vs (vals publics)
                                                     vs (map deref vs)
                                                     publics (zipmap ks vs)
                                                     publics (if refer
                                                               (select-keys publics refer)
                                                               publics)]
                                                 publics)))]
                               (.then macros
                                      (fn [macros]
                                        (set! compiler/built-in-macros
                                              ;; hack
                                              (merge compiler/built-in-macros macros))))))))
                  (js/Promise.resolve nil)
                  require-macros))))))

(defn compile-string* [contents opts]
  (-> (js/Promise.resolve (scan-macros contents))
      (.then #(compiler/compile-string* contents opts))))

(defn compile-file [{:keys [in-file out-file extension] :as opts}]
  (let [contents (slurp in-file)]
    (-> (compile-string* contents opts)
        (.then (fn [{:keys [javascript jsx]}]
                 (let [out-file (or out-file
                                    (str/replace in-file #".clj(s|c)$"
                                                 (if jsx
                                                   ".jsx"
                                                   (or (when-let [ext extension]
                                                         (str "." (str/replace ext #"^\." "")))
                                                       ".mjs"))))]
                   (spit out-file javascript)
                   {:out-file out-file}))))))

(defn compile-file-js
  "Same as compile-file but expects JS object as arg and returns JS
  object. Used for exposing to JS."
  [m]
  (let [res (compile-file (js->clj m :keywordize-keys true))]
    (clj->js res)))

(defn compile-string-js
  [contents]
  (let [res (compile-string* contents nil)]
    (.then res (fn [m]
                 (prn :m m)
                 (:javascript m)))))
