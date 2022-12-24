(ns tasks
  (:require [babashka.curl :as curl]
            [cheshire.core :as json]))

(defn pull-request [{:keys [github-token branch]}]
  (let [headers {:headers {"Accept" "application/vnd.github+json"
                           "Authorization" (str "Bearer " github-token)
                           "X-GitHub-Api-Version" "2022-11-28"}}
        resp (curl/post "https://api.github.com/repos/squint-cljs/squint/pulls"
                        headers
                        :body (json/generate-string {:title "Bump common"
                                                     :body "Bump common"
                                                     :head branch
                                                     :base "main"}))
        body (:body resp)
        body (json/parse-string body true)
        url (:url body)
        comment-url (str url "/comments")]
    (curl/post comment-url
               headers
               :body (json/generate-string {:body "@borkdude: please merge!"}))))
