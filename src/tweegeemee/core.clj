(ns tweegeemee.core
  (:use [clisk live])
  (:require [twitter.api.restful :as tw]
            [twitter.oauth :as tw-oauth]
            [twitter.request :as tw-req]
            [environ.core :refer [env]]
            [clojure.zip :as zip]
            [clojure.set :as set]
            [tentacles.gists :as gists])
  (:import [java.io File]
           [javax.imageio ImageIO]))

(def my-creds (tw-oauth/make-oauth-creds
               (env :app-consumer-key)
               (env :app-consumer-secret)
               (env :user-access-token)
               (env :user-access-secret)))

(def my-screen-name (env :screen-name))

(def my-gist-auth (env :gist-auth))
(def my-gist-archive-id (env :gist-archive-id))

(def test-size 16)
(def full-size 720)

;; Functions for use in creating imagery.  Get more in clisk
(def unary-fns #{'vsin 'vcos 'vabs 'vround 'vfloor 'vfrac
                 'square 'vsqrt 'sigmoid 'max-component 'min-component
                 'length 'normalize})
(def binary-fns #{'v+ 'v* 'v- 'vdivide 'vmin 'vmax})
(def fns (set/union unary-fns binary-fns))

(defn random-fn
  "return a random function.  Parameter n selects either 1 or 2 parameters."
  [n]
  (if (= n 2)
    (rand-nth (seq binary-fns))
    (rand-nth (seq unary-fns))))
;;(random-fn)

(defn rand-value
  "return a random value between -1,1 with only 3 significant digits
  to increase tweetability"
  []
  (let [x (- (rand 2) 1)
        x (/ (Math/floor (* x 1000)) 1000.0)]
    x))

(defn random-terminal
  "return a random terminal value: vectors, position, or noise."
  []
  (let [r (rand)]
    (if (< r 0.10)
      'T0
      (if (< r 0.15)
        'T1
      (if (< r 0.2)
        'T2
        (if (< r 0.5)
          (rand-value)
          (if (< r 0.75)
            [(rand-value) (rand-value)]
            [(rand-value) (rand-value) (rand-value)])))))))
;;(random-terminal)

(defn random-code
  "return a random s-expression made up of functions or terminals"
  ([] (random-code 2))
  ([depth]
   (if (and (> depth 0)
            (> (rand-int depth) 0))
     (if (< 0.5 (rand))
       (cons (random-fn 2)
             (repeatedly 2 #(random-code (dec depth))))
       (cons (random-fn 1)
             (repeatedly 1 #(random-code (dec depth)))))
     (random-terminal))))
;;(random-code 5)

(defn locs
  "return all zip locations within the s-expression.  each location
  contains the full context within the tree for use in replacement
  later."
  [G]
  (let [zipper (zip/seq-zip G)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    (filter #(not (fns (zip/node %))) all-locs)))

(defn replace-loc
  "replace the location loc1 with the location loc2, returning the root (full
  s-expression) of loc1."
  [loc1 loc2]
  (zip/root (zip/replace loc1 (zip/node loc2))))

(defn breed
  "find a random expression in L to replace with a random expression
  in R.  replace it within L and return a new L s-expression."
  [L R]
  (let [loc1 (rand-nth (locs L))
        loc2 (rand-nth (locs R))]
    (replace-loc loc1 loc2)))

(defn good-random-code?
  "does it have at least one paren?"
  [code]
  (let [code-str (str code)]
    (and (>= (count (filter #(= % \( ) code-str)) 1))))

(defn good-image?
  "is the image not a constant color?"
  [img]
  (let [values (for [x (range test-size) y (range test-size)] (.getRGB img x y))
        min-v (apply min values)
        max-v (apply max values)]
    (> (- max-v min-v) 0)))

(defn get-good-random-code
  []
  (let [good-image (atom false)
        good-code (atom nil)]
    (while (not @good-image)
      (try
        (let [gc (random-code 10)
              _ (when (not (good-random-code? gc))
                  (/ 0)) ;; cause exception
              ;;_ (prn gc)
              img (image (eval gc) :size test-size)]
          ;; no exception
          (when (good-image? img)
            (reset! good-image true)
            (reset! good-code gc)))
        (catch Exception e
          (prn "not good code" e)
          nil)))
    @good-code))

(defn write-png
  ""
  [filename the-image]
  (ImageIO/write the-image "png" (File. filename)))

(defn- get-timestamp-str
  []
  (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss") (System/currentTimeMillis)))

(defn- get-png-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".png"))

(defn- get-clj-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".clj"))

(defn- get-clj-basename
  [timestamp suffix]
  (str timestamp "_" suffix ".clj"))

(defn make-random-code-and-png
  [timestamp suffix]
  (let [my-code (get-good-random-code)
        png-filename (get-png-filename timestamp suffix)
        my-image (image (eval my-code) :size full-size) ;; FIXME AA?
        clj-filename (get-clj-filename timestamp suffix)
        clj-basename (get-clj-basename timestamp suffix)]
    (write-png png-filename my-image)
    (spit clj-filename (str my-code))
    [my-code clj-basename png-filename]))

(defn post-to-twitter
  [status-text the-image-filename]
  (try
    (if false
      (prn "NOT posting to twitter" status-text)
      (tw/statuses-update-with-media
       :oauth-creds my-creds
       :body [(tw-req/file-body-part the-image-filename)
              (tw-req/status-body-part status-text)]))
    (catch Exception e
      ;; FIXME -- why does this always have a remote-closed exception?
      (prn "caught twitter exception" e)))
  (prn "waiting for a bit...")
  (Thread/sleep 5000))

(defn append-to-gist
  [filename content]
  (gists/edit-gist
   my-gist-archive-id
   {:auth my-gist-auth
    :files { filename { :content content }}}))
;; (append-to-gist "TEST.txt" "another test 1")

(defn post-to-web
  [the-code clj-filename png-filename]
  (let [status-text (str clj-filename " https://gist.githubusercontent.com/rogerallen/"
                         my-gist-archive-id "/raw/" clj-filename)]
  (append-to-gist clj-filename (str the-code))
  (post-to-twitter status-text png-filename)))

(defn post-random-batch-to-web
  "Post a batch of 5 random codes & images to twitter"
  []
  (let [timestamp-str (get-timestamp-str)]
    (dorun
     (doseq [suffix "abcde"]
       (let [[the-code clj-filename png-filename] (make-random-code-and-png timestamp-str suffix)]
         (post-to-web the-code clj-filename png-filename))))))

;; FIXME BELOW!!!!

(defn score-status
  "retweets count more than favorites"
  [status]
  (+ (* 3 (:retweet_count status)) (:favorite_count status)))

(defn get-top-scoring-codes
  []
  (let [statuses (->> (:body (tw/statuses-user-timeline
                              :oauth-creds my-creds
                              :params {:count 10
                                       :screen-name my-screen-name}))
                      (map #(assoc % :text
                                   (clojure.string/replace (:text %) #" http.*" "")))
                      (filter #(good-random-code? (:text %)))
                      (map #(assoc % :score (score-status %)))
                      (sort-by :score)
                      (reverse)
                      (take 2)
                      (map :text)
                      (map read-string))] ;; DANGER!! FIXME sanity check!!
    statuses))

(defn get-good-random-child
  [code0 code1]
  (let [good-image (atom false)
        good-code (atom nil)]
    (while (not @good-image)
      (try
        (let [gc (breed code0 code1)
              _ (prn gc)
              _ (when (not (good-random-code? gc))
                  (/ 0)) ;; cause exception
              img (image (eval gc) :size test-size)]
          ;; no exception
          (when (good-image? img)
            (reset! good-image true)
            (reset! good-code gc)))
        (catch Exception e
          (prn "not good code" e)
          nil)))
    @good-code))

(defn get-good-random-child-and-png
  [code0 code1 suffix]
  (let [my-code (get-good-random-child code0 code1)
        my-filename (get-png-filename suffix)
        my-image (image (eval my-code) :size full-size)] ;; AA?
     (write-png my-filename my-image)
    [my-code my-filename]))

(defn post-children-to-twitter
  "Post a batch of 5 random codes & images to twitter"
  []
  (let [[c0 c1] (get-top-scoring-codes)]
    (dorun
     (doseq [f "ABCDE"]
       (let [[c f] (get-good-random-child-and-png c0 c1 f)
             _ (prn c)]
         nil;;(post-to-twitter c f)
         )))))


(comment

  ;; GIST STUFF ===========================================
  (require '[clojure.pprint :as pp])
  (require '[tentacles.users :as users])
  (pp/pprint (users/user "rogerallen"))
  (pp/pprint (users/me {:auth my-gist-auth}))
  (require '[tentacles.gists :as gists])
  (pp/pprint (gists/specific-gist my-gist-archive-id))
  (gists/edit-gist my-gist-archive-id
                   { :auth my-gist-auth
                    :files
                    { "README.txt"
                      { :content "Archive the code for https://twitter.com/tweegeemee.\nThis was edited." },
                      "TEST.txt"
                      { :content "a new file" }}})

  ;; TWITTER STUFF ========================================

  ;; got some info about me
  (tw/users-show :oauth-creds my-creds
                 :params {:screen-name my-screen-name})

  (def last-status-id (-> (tw/users-show :oauth-creds my-creds
                                         :params {:screen-name my-screen-name})
                          :body
                          :status
                          :id))
  last-status-id

  ;; get statuses & favorites
  (defn print-last-n-statuses [N]
    (doall (doseq [s (:body (tw/statuses-user-timeline
                             :oauth-creds my-creds
                             :params {:count N
                                      :screen-name my-screen-name}))]
             (println (:retweet_count s) (:favorite_count s) (:text s)))))
  (print-last-n-statuses 5)

  ;; coolio
  (tw/statuses-update
   :oauth-creds my-creds
   :params {:status "Testing a post from the REPL."})

  ;; double-coolio
  (tw/statuses-update-with-media
   :oauth-creds my-creds
   :body [(tw-req/file-body-part "test0004.png")
          (tw-req/status-body-part "An image to start with")])


  (U0 (B3 (B2 [-0.658 -0.552] (B3 (U3 (Ua (Uc (U8 0.045)))) (Uc (Uc T0)))) (B4 0.328 (B5 -0.228 (U6 (U9 T1)))))) ;; 110 in length 140-24 = 116

  )
