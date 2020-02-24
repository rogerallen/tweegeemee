(ns tweegeemee.explore
  (:use [clisk live])
  (:require
   [tweegeemee.core    :as tgm]
   [tweegeemee.twitter :as twitter]
   [tweegeemee.gists   :as gists]))

;; ======================================================================
;; ideas for facebook/tumbler/etc posts
;; 1) last week's Top 3
;; 2) monthly mosaic image.  30 days x 24 images/day (32x32px?)
;; 3) album: ancestry of last week's post (or a post people talk about)
;; ======================================================================

;; Postprocess with e.g.:
;;   montage -tile 48x30 -geometry 32x32 1512*png a.png
(defn- render-a-months-statuses
  [year month]
  (let [statuses (twitter/get-a-months-statuses tgm/my-twitter-creds tgm/my-screen-name year month)
        data     (gists/read-archive tgm/my-gist-auth tgm/my-gist-archive-id)] ;; FIXME take archive name
    (doseq [s statuses]
      (let [name         (clojure.string/replace (:text s) #" http.*" "")
            my-code      (:code (gists/get-entry-by-name data name))
            _            (println name "::" my-code)
            my-image     (clisk.live/image (eval my-code) :size 32)
            png-filename (str "images/mosaic/" name ".png")]
        (tgm/write-png png-filename my-image)))))

(defn render-statuses
  [names size]
  (let [data (gists/read-archive tgm/my-gist-auth tgm/my-gist-archive-id)] ;; FIXME take archive name
    (doseq [name names]
      (let [my-code      (:code (gists/get-entry-by-name data name))
            _            (println name "::" my-code)
            my-image     (clisk.live/image (eval my-code) :size size)
            png-filename (str "images/tiles/" name ".png")]
        (tgm/write-png png-filename my-image)))))

(defn- get-top-n
  "return a sequence of the N highest-scoring tweet image links."
  [fn N]
  (let [statuses (->> (fn);; (get-todays-statuses)
                      (filter #(re-matches #"\d\d\d\d\d\d_\d\d\d\d\d\d_\w+.clj .*" (:text %)))
                      (map #(assoc % :score (tgm/score-status %)))
                      (map #(dissoc % :user :extended_entities))
                      (sort-by :score)
                      (reverse)
                      (take N))]
    statuses))

;; (get-top-n get-last-weeks-statuses 3)
(defn print-top-n
  [fn N]
  (doseq [s (get-top-n fn N)]
    (let [score (:score s)
          url   (-> s :entities :media first :expanded_url)]
      (println score url))))
;;(print-top-n get-last-weeks-statuses 3)

;; geneaology
(defn- update-layout
  [layout child parent-map generation y]
  (let [parents (parent-map child)
        num-parents (count parents)]
    ;;(println generation y (count parents) child)
    ;; if I was smarter, maybe I'd be able to conj this below...
    (swap! layout #(conj % [generation y num-parents child]))
        (if (empty? parents)
          [(inc y) [generation y num-parents child]]
          (let [generation (inc generation)]
            (reduce #(do ;; (println "x" y %1 %2)
                       (update-layout layout (first %2) parent-map generation (max (first %1) (+ y (second %2)))))
                    [y []]
                    (map vector parents (range num-parents)))))))

(defn layout-generations
  [child parent-map]
  (let [layout (atom [])] ;; vector of vectors [x y num-parents name]
    (update-layout layout child parent-map 0 0)
    (sort @layout)))

;; ======================================================================
;; code for interactively exploring tweegeemee possibilities
;; ======================================================================
;; (ns tweegeemee.explore)

(comment

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
  (twitter/post-status tgm/my-twitter-creds "Hey, is this on? Testing!")

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
  )
