(ns tweegeemee.core
  (:use [clisk live])
  (:require
   ;;[clisk.live] tried this, but caused issues
   [clisk.patterns]
   [tweegeemee.image   :as image]
   [environ.core       :refer [env]]
   [clojure.java.io    :as io]
   [clojure.java.jdbc  :as sql])
  (:import [java.io File]
           [javax.imageio ImageIO])
  (:gen-class))

;; ======================================================================
(defonce NUM-PARENT-TWEETS 200)   ;; 24 posts/day * 2 imgs = 48 img/day = ~4 days
(defonce MAX-POSSIBLE-PARENTS 10) ;; top 10 tweets become parents

;; ======================================================================
(defonce my-pg-db            (atom nil))
(defonce my-old-hashes       (atom nil))
(defonce my-old-image-hashes (atom nil))

;; ======================================================================
(defn write-png
  "write the-image to filename (which should have a .png suffix)"
  [filename the-image]
  (ImageIO/write the-image "png" (File. filename)))

(defn- get-timestamp-str
  "return a timestamp for the current time"
  []
  (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss") (System/currentTimeMillis)))

(defn- get-png-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".png"))

(defn- get-clj-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".clj"))

(defn- get-json-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".json"))

(defn- get-clj-basename
  [timestamp suffix]
  (str timestamp "_" suffix ".clj"))

(defn- make-code-and-png*
  "make code from code-fn and save as files. return the code,
  clj-filename and png-filename"
  [code-fn timestamp suffix]
  (let [my-code (code-fn)]
    (if (nil? my-code)
      [nil nil nil]
      (let [png-filename (get-png-filename timestamp suffix)
            my-image     (clisk.live/image (eval my-code) :size image/IMAGE-SIZE)
            clj-filename (get-clj-filename timestamp suffix)
            json-filename (get-json-filename timestamp suffix)
            clj-basename (get-clj-basename timestamp suffix)]
        (write-png png-filename my-image)
        (spit clj-filename (pr-str my-code))
        [my-code clj-basename png-filename json-filename]))))

(declare get-random-code)
(declare get-random-child)
(declare get-random-mutant)
(defn- make-random-code-and-png
  "make random code and save as files. return the code, clj-filename,
  png-filename and json-filename"
  [timestamp suffix]
  (make-code-and-png* get-random-code timestamp suffix))

(defn- make-random-child-and-png
  "take 2 codes, breed them and save as files. return the code,
  clj-filename, png-filename and json-filename"
  [code0 code1 timestamp suffix]
  (make-code-and-png* (partial get-random-child code0 code1)
                      timestamp suffix))

(defn- make-random-mutant-and-png
  "take a code, mutate it and save as files. return the code,
  clj-filename, png-filename and json-filename"
  [code timestamp suffix]
  (make-code-and-png* (partial get-random-mutant code)
                      timestamp suffix))

(defn- cur-hour
  "return the current hour"
  []
  (.get (java.util.Calendar/getInstance) java.util.Calendar/HOUR_OF_DAY))

;; fix https://github.com/rogerallen/tweegeemee/issues/14
;; add twitter id to gist data
(defn- find-twitter-id
  "find this entry's :id_str (twitter-id) inside the statuses by matching
  the :name of the entry to the start of the status :text.  if not found,
  return nil."
  [entry statuses]
  (let [regex  (re-pattern (str "^" (:name entry)))
        status (first (filter #(re-find regex (:text %)) statuses))]
    (if (not (empty? status))
      (:id_str status)
      nil)))

;; ======================================================================
;; Public API ===========================================================
;; ======================================================================
;; add these keys to your environment or
;; profiles.clj AND DON'T CHECK THAT FILE IN !!!
;; # posgresql
;; export DB_NAME="FIXME"
;; export DB_USER="FIXME"
;; export DB_USER_PASS="FIXME"
;; export DB_HOST="FIXME"
;; export DB_PORT="FIXME"
;; # just pick a random integer (also used as a "generation" number)
;; export CLISK_RANDOM_SEED="FIXME"
(defn setup-env!
  "setup environment vars"
  []
  (reset! my-pg-db {:dbtype "postgresql"
                    :dbname (env :db-name)
                    :host (env :db-host)
                    :port (env :db-port)
                    :user (env :db-user)
                    :password (env :db-user-pass)})
  (when-let [seed (Integer/parseInt (env :clisk-random-seed))]
    (println "clisk-random-seed" seed)
    (clisk.patterns/seed-perlin-noise! seed)
    (clisk.patterns/seed-simplex-noise! seed))
  nil)

(defn- set-old-hashes-from-db!
  "return a vector of :hash and :image-hash data from the DB."
  []
  (let [random-seed (Integer/parseInt (env :clisk-random-seed))
        data (sql/query
              @my-pg-db
              ["SELECT hash,image_hash FROM items WHERE random_seed = ?"
               random-seed])
        old-hashes (set (map :hash data))
        old-image-hashes (set (map :image_hash data))
        _ (println "Current generation seed" random-seed "has" (count data) "items.")]
    (reset! my-old-hashes old-hashes)
    (reset! my-old-image-hashes old-image-hashes)))

(defn get-random-code
  "get a good image-creation code created randomly"
  []
  (image/get-random-code my-old-hashes my-old-image-hashes))

(defn get-random-child
  "get a good image-creation code created via breeding two other
  codes"
  [code0 code1]
  (image/get-random-child my-old-hashes my-old-image-hashes code0 code1))

(defn get-random-mutant
  "get a good image-creation code created via mutating a code"
  [code]
  (image/get-random-mutant my-old-hashes my-old-image-hashes code))

(defn score-status
  "return a score for a image's tweet based on favorites and 
   retweets. retweets count more than favorites
   Updated for Mastodon & Tumblr retweets & favorites."
  [status]
  (+ (* 3 (+ (:retweet_count status) (:retweet_count1 status) (:retweet_count2 status)))
     (+ (:favorite_count status) (:favorite_count1 status)  (:favorite_count2 status))))

(defn get-parent-tweets-from-db-new
  "return a sequence of the N highest-scoring tweets.  Tweet data is
   from the local database and should match the random-seed.  No longer
   using the gist website."
  [random-seed N]
  (let [statuses (->> (sql/query
                       @my-pg-db
                       [;;"SELECT * FROM items WHERE random_seed = ? ORDER BY key DESC LIMIT ?"
                        ;; updated to use the items + scores table
                        "SELECT sum(scores.favorite_count) + 3*sum(scores.retweet_count) as score,
                                items.code as code
                         FROM items INNER JOIN scores ON items.key = scores.item_key
                         WHERE items.random_seed = ? 
                         GROUP BY items.key, items.code
                         ORDER BY items.key DESC 
                         LIMIT ?"
                        random-seed NUM-PARENT-TWEETS])
                      ;;(map #(assoc % :score (score-status %)))
                      (sort-by :score)
                      (reverse)
                      (take N)
                      (map #(update-in % [:code] (fn [x] (read-string (str x))))))]
    statuses))

(defn save-locally
  "Save the-code & info to a local directory. 
   clj-file png-file have already been saved"
  [the-code json-filename parent-vec]
  (let [image-hash (image/image-hash (clisk.live/image (eval the-code) :size image/TEST-IMAGE-SIZE))
        code-hash (hash the-code)
        random-seed (env :clisk-random-seed)
        parent0 (first parent-vec)
        parent1 (first (rest parent-vec))
        ;; a bit of a hack, but it works...
        my-json-str (str
                     "{\n"
                     "    \"parents\": ["
                     (if (nil? parent0)
                       ""
                       (if (nil? parent1)
                         (str "\"" parent0 "\"")
                         (str "\"" parent0 "\", \"" parent1 "\"")))
                     "],\n"
                     "    \"hash\": " (str code-hash) ",\n"
                     "    \"image_hash\": " (str image-hash) ",\n"
                     "    \"random_seed\": " random-seed "\n"
                     "}\n")]
    (spit json-filename my-json-str)))

(defn make-batch*
  "Make & save a batch of random codes & images, plus save info about them in a json file."
  [make-fn post-str suffix-str parent-vec]
  (dorun
   (doseq [suffix suffix-str]
     (let [_ (println post-str "begin" suffix)
           [the-code _ _ json-filename] (make-fn suffix)]
       (if (nil? the-code)
         (println "!!! suffix" suffix "unable to create" post-str "image")
         (save-locally the-code json-filename parent-vec)))))
  (println post-str "done."))

(defn make-random-batch
  "Make a batch of random codes & images."
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)]
    (make-batch* (partial make-random-code-and-png timestamp-str)
                 "random" suffix-str [])))

(defn rand-nth-or-nil
  "protect from empty list rand-nth"
  [lst]
  (if (= 0 (count lst))
    nil
    (rand-nth lst)))

(defn make-children
  "Find the highest-scoring parents, make a batch of codes & images
  bred from those parents"
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)
        rents         (get-parent-tweets-from-db-new (Integer/parseInt (env :clisk-random-seed)) MAX-POSSIBLE-PARENTS)
        c0            (rand-nth-or-nil rents)
        c1            (rand-nth-or-nil rents)
        can-work      (and (some? c0) (some? c1))]
    (if can-work
      (make-batch* (partial make-random-child-and-png (:code c0) (:code c1) timestamp-str)
                   "repro" suffix-str [(:name c0) (:name c1)])
      (println "INFO: No parents.  Cannot create child to post."))))

(defn make-mutants
  "Find the highest-scoring parents, post a batch of 5 codes & images
  mutated from the top parent to twitter and github"
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)
        rents         (get-parent-tweets-from-db-new (Integer/parseInt (env :clisk-random-seed)) MAX-POSSIBLE-PARENTS)
        c0            (rand-nth-or-nil rents)
        can-work      (some? c0)]
    (if can-work
      (make-batch* (partial make-random-mutant-and-png (:code c0) timestamp-str)
                   "mutant" suffix-str [(:name c0)])
      (println "INFO: No parent.  Cannot create mutant to post."))))

;; ======================================================================
;; main entry
;; ======================================================================
(defn main [args]
  (println "posting one of: (0) random ab, (1) children CD or (2) mutant MN")
  (setup-env!)
  (let [gen (if (empty? args)
              (mod (cur-hour) 3)
              (Integer/parseInt (first args)))]
    (set-old-hashes-from-db!)
    (case gen
      0 (make-random-batch "ab")
      1 (make-children "CD")
      2 (make-mutants "MN"))
    (println "posting complete.")
    (shutdown-agents) ;; quit faster
    (println "shutdown agents")
    0))

(defn -main [& args]
  (println "======================================================================")
  (println "Started version" (env :tweegeemee-version))
  (if (seq args)
    (if (= (first args) "new")
      (main (rest args))
      (printf "use 'lein run new [#]'"))
    (printf "original-main is unavailable. use 'lein run new [#]'")))
