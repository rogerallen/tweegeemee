(ns tweegeemee.explore
  (:use [clisk live])
  (:require
   [tweegeemee.core    :as tgm]
   [tweegeemee.twitter :as twitter]
   [tweegeemee.gists   :as gists]))

;; ======================================================================
;; code for interactively exploring tweegeemee possibilities

;; ======================================================================
;; Example usage to explore at the repl
;; ======================================================================
;; (ns tweegeemee.explore)

;; be sure to get this setup
(tgm/setup-env!)

;; generate & show a random image
(let [c (tgm/get-random-code)]
  (show (eval c)))

;; breed images by hand...
(def rents (tgm/get-parent-tweets 5))
(def dad (:code (rand-nth rents)))
(def mom (:code (rand-nth rents)))
(show (eval dad))
(show (eval mom))
(let [c (tgm/get-random-child dad mom)]
  (if (not (nil? c))
    (show (eval c))))

;; breed mutants by hand...
(def rents (tgm/get-parent-tweets 5))
(def dad (:code (rand-nth rents)))
(show (eval dad))
(let [c (tgm/get-random-mutant dad)]
  (if (not (nil? c))
    (show (eval c))))

;; Look at the frequency of instructions
(def archive (gists/read-archive tgm/my-gist-auth tgm/my-gist-archive-id))
(defn get-fns [s]
  (filter #(not= % "")
          (-> (clojure.string/replace s #"clisk.live/|\(|\)|\[|\]|\.|[0-9]" "")
              (clojure.string/replace #" -" "")
              (clojure.string/split #" "))))
(println (sort-by val (frequencies (sort (mapcat #(get-fns (:code %)) archive)))))
(println (sort-by val (frequencies (sort (mapcat #(get-fns (:code %)) (drop 200 archive))))))

;; ----------------------------------------------------------------------
;; NOTE that code below this point posts to the web
;; Careful!
(tgm/post-random-batch-to-web "r")

;; Careful!
(tgm/post-children-to-web "AB")

;; Careful!
(tgm/post-mutants-to-web "M")

;; post a status
(twitter/post-status tgm/my-twitter-creds "Hey, is this on?")

;; genealogy
(def rents (tgm/get-parent-tweets 5))
(def data (gists/read-archive tgm/my-gist-auth tgm/my-gist-archive-id))

(def parent-map (apply hash-map
                       (mapcat
                        #(let [v (select-keys % [:name :parents])]
                           [(:name v) (:parents v)])
                        data)))
(defn get-by-name [name] (first (filter #(= name (:name %)) data)))

(def cur (nth rents 1))
(show (eval (:code cur)))
(def cur (get-by-name (first (:parents cur))))
(def cur (get-by-name (second (:parents cur))))

(show (eval (:code (nth rents 1))) :width 900 :height 900)

;; Facebook update helpers
(tgm/print-top-n
 (partial twitter/get-todays-statuses tgm/my-twitter-creds tgm/my-screen-name)
 5)

;; geneaology (start with a good name)
(def layout (tgm/layout-generations "200223_100825_D.clj" parent-map))

;; render tiles (mkdir images/tiles)
(tgm/render-statuses (sort (distinct (map #(nth % 3) layout))) 64)

;; print for use in ipython to create the final image
(map #(println (apply format "[%d, %d, %d, '%s']," %)) layout)

;; rate limit issue?
;;(-> (tw/application-rate-limit-status :oauth-creds @my-twitter-creds)
;;    :body
;;    :resources
;;    :statuses
;;    :/statuses/user_timeline)
;; {:limit 180, :remaining 180, :reset 1455205245}
