(ns tasks
  (:require
   [babashka.fs :as fs]
   [babashka.process :refer [shell]]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [node-repl-tests]
   [clojure.string :as str]
   [babashka.curl :as curl]))

(defn munge* [s reserved]
  (let [s (str (munge s))]
    (if (contains? reserved s)
      (str s "$")
      s)))

(defn shadow-extra-config
  []
  (let [core-config (edn/read-string (slurp (io/resource "squint/cljs.core.edn")))
        reserved (edn/read-string (slurp (io/resource "squint/js_reserved.edn")))
        vars (:vars core-config)
        ks (map #(symbol (munge* % reserved)) vars)
        vs (map #(symbol "cljs.core" (str %)) vars)
        core-map (zipmap ks vs)
        core-map (assoc core-map 'goog_typeOf 'goog/typeOf)]
    {:modules
     {:cljs_core {:exports core-map}}}))

(def test-config
  '{:compiler-options {:load-tests true}
    :modules {:squint_tests {:init-fn squint.compiler-test/init
                             :depends-on #{:compiler}}}})

(defn shadow-extra-test-config []
  (merge-with
   merge
   (shadow-extra-config)
   test-config))

(defn bump-core-vars []
  (let [core-vars (:out (shell {:out :string}
                               "node --input-type=module -e 'import * as squint from \"squint-cljs/core.js\";console.log(JSON.stringify(Object.keys(squint)))'"))
        parsed (apply sorted-set (map symbol (json/parse-string core-vars)))]
    (spit "resources/squint/core.edn" (with-out-str
                                        ((requiring-resolve 'clojure.pprint/pprint)
                                         parsed)))))

(defn build-squint-npm-package []
  (fs/create-dirs ".work")
  (fs/delete-tree "lib")
  (fs/delete-tree ".shadow-cljs")
  (bump-core-vars)
  (spit ".work/config-merge.edn" (shadow-extra-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release squint"))

(defn publish []
  (build-squint-npm-package)
  (run! fs/delete (fs/glob "lib" "*.map"))
  (shell "npm publish"))

(defn watch-squint []
  (fs/create-dirs ".work")
  (fs/delete-tree ".shadow-cljs/builds/squint/dev/ana/squint")
  (spit ".work/config-merge.edn" (shadow-extra-test-config))
  (bump-core-vars)
  (shell "npx shadow-cljs --aliases :dev --config-merge .work/config-merge.edn watch squint"))

(defn test-squint []
  (fs/create-dirs ".work")
  (spit ".work/config-merge.edn" (shadow-extra-test-config))
  (bump-core-vars)
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release squint")
  (shell "node lib/squint_tests.js")
  (node-repl-tests/run-tests {}))

(defn bump-compiler-common [{:keys [sha]}]
  (let [rdissoc (requiring-resolve 'borkdude.rewrite-edn/dissoc)
        rupdate-in (requiring-resolve 'borkdude.rewrite-edn/update-in)
        deps (slurp "deps.edn")
        nodes ((requiring-resolve 'borkdude.rewrite-edn/parse-string) deps)
        nodes ((requiring-resolve 'borkdude.rewrite-edn/assoc-in) nodes [:deps 'io.github.squint-cljs/compiler-common :git/sha] sha)
        nodes ((requiring-resolve 'borkdude.rewrite-edn/assoc-in) nodes [:deps 'io.github.squint-cljs/compiler-common :deps/root] "compiler-common")
        nodes (rupdate-in nodes [:deps 'io.github.squint-cljs/compiler-common]
                          rdissoc :local/root)
        deps (str nodes)]
    (spit "deps.edn" deps)))

(defn pull-request [{:keys [github-token branch]}]
  (let [headers {"Accept" "application/vnd.github+json"
                 "Authorization" (str "Bearer " github-token)
                 "X-GitHub-Api-Version" "2022-11-28"}
        resp (curl/post "https://api.github.com/repos/squint-cljs/squint/pulls"
                        {:headers headers
                         :body (json/generate-string {:title "Bump common"
                                                      :body "Bump common"
                                                      :head branch
                                                      :base "main"})})
        body (:body resp)
        body (json/parse-string body true)
        url (:issue_url body)
        comment-url (str url "/comments")]
    (curl/post comment-url
               {:headers headers
                :body (json/generate-string {:body "@borkdude: please merge!"})})))
