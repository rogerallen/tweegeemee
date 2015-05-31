(ns tweegeemee.core
  (:require [twitter.api.restful :as twitter]
            [twitter.oauth :as twitter-oauth]
            [twitter.request :as twitter-request]
            [environ.core :refer [env]]))

(def my-creds (twitter-oauth/make-oauth-creds
               (env :app-consumer-key)
               (env :app-consumer-secret)
               (env :user-access-token)
               (env :user-access-secret)))

(comment

  ;; got some info about me
  (twitter/users-show :oauth-creds my-creds
                      :params {:screen-name "tweegeemee"})

  (def last-status-id (-> (twitter/users-show :oauth-creds my-creds
                                              :params {:screen-name "tweegeemee"})
                          :body
                          :status
                          :id))
  last-status-id

  ;; get statuses & favorites
  (defn print-last-n-statuses [N]
    (doall (doseq [s (:body (twitter/statuses-user-timeline
                             :oauth-creds my-creds
                             :params {:count N
                                      :screen-name "tweegeemee"}))]
             (println (:retweet_count s) (:favorite_count s) (:text s)))))
  (print-last-n-statuses 5)

  ;; coolio
  (twitter/statuses-update
   :oauth-creds my-creds
   :params {:status "Testing a post from the REPL."})

  ;; double-coolio
  (twitter/statuses-update-with-media
   :oauth-creds my-creds
   :body [(twitter-request/file-body-part "test0004.png")
          (twitter-request/status-body-part "An image to start with")])

  )
