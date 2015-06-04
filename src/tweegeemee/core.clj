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

;; ======================================================================
;; add these keys to your profiles.clj (AND DON'T CHECK THAT FILE IN!)
(def my-creds (tw-oauth/make-oauth-creds
               (env :app-consumer-key)          ;; all from api.twitter.com
               (env :app-consumer-secret)
               (env :user-access-token)
               (env :user-access-secret)))
(def my-screen-name     (env :screen-name))     ;; twitter screen name
(def my-gist-auth       (env :gist-auth))       ;; gist username:password
(def my-gist-archive-id (env :gist-archive-id)) ;; create this archive

;; ======================================================================
(def min-color-value 32)      ;; not too dark
(def min-color-difference 10) ;; not too similar
(def test-size 16)
(def full-size 720)
(def max-code-depth 6)

;; Functions for use in creating imagery.
(declare random-value)
(defn- random-scalar [] (random-value))
(defn- random-vec2 [] [(random-value) (random-value)])
(defn- random-vec3 [] [(random-value) (random-value) (random-value)])
(defn- random-vec4 [] [(random-value) (random-value) (random-value) (random-value)])
(def term-vals #{'pos random-scalar random-vec2 random-vec3 random-vec4})
(def term-fns #{'noise 'snoise 'plasma 'splasma
                'vnoise 'vsnoise 'vplasma 'vsplasma
                'grain 'turbulence 'vturbulence
                'spots 'blotches})
(def unary-fns #{'vsin 'vcos 'vabs 'vround 'vfloor 'vfrac
                 'square 'vsqrt 'sigmoid 'max-component 'min-component
                 'length 'normalize 'gradient
                 'hue-from-rgb 'lightness-from-rgb 'saturation-from-rgb
                 'hsl-from-rgb 'red-from-hsl 'green-from-hsl 'blue-from-hsl
                 'rgb-from-hsl 'x 'y 'z 't 'alpha })
(def binary-fns #{'v+ 'v* 'v- 'vdivide 'vpow 'vmod 'dot 'cross3
                  'vmin 'vmax 'checker 'scale 'offset
                  'adjust-hue 'adjust-hsl 'vconcat})
(def ternary-fns #{'lerp 'clamp})
(def fns (set/union unary-fns binary-fns ternary-fns))
;; Probabilities
(def prob-term-fn    0.1)  ;; vs terminal values
(def prob-ternary-fn 0.02) ;; vs. binary or unary
(def prob-binary-fn  0.3)  ;; vs ternary or unary

;; ======================================================================
(defn- random-fn
  "return a random function.  Parameter n selects either 1 or 2 parameters."
  [n]
  (case n
    3 (rand-nth (seq ternary-fns))
    2 (rand-nth (seq binary-fns))
    1 (rand-nth (seq unary-fns))))
;;(random-fn 3)

(defn- random-value
  "return a random value in the range (-3,3) with only 4 significant digits
  to increase readability"
  []
  (let [x (* 3 (- (rand 2) 1))
        x (/ (Math/floor (* x 10000)) 10000.0)]
    x))
;;(random-value)

(defn- random-terminal
  "return a random terminal value: vectors, position, or noise."
  []
  (if (< (rand) prob-term-fn)
    (rand-nth (seq term-fns))
    (let [x (rand-nth (seq term-vals))]
      (if (not= x 'pos) (x) x))))
;;(random-terminal)

(defn- random-code
  "Recursively create & return a random s-expression made up of
  functions or terminals. When depth=0, create a terminal to control
  the size.  Create terminal fn with increasing probability as depth
  gets smaller.  Functions are not parameter-checked so runtime
  exceptions can be expected."
  ([depth]
   (if (and (> depth 0) (> (rand-int depth) 0))
     (if (< (rand) prob-ternary-fn)
       (cons (random-fn 3) (repeatedly 3 #(random-code (dec depth))))
       (if (< (rand) prob-binary-fn)
         (cons (random-fn 2) (repeatedly 2 #(random-code (dec depth))))
         (cons (random-fn 1) (repeatedly 1 #(random-code (dec depth))))))
     (random-terminal))))
;;(random-code 5)

(defn- locs
  "return all zip locations within the s-expression.  each location
  contains the full context within the tree for use in replacement
  later."
  [G]
  (let [zipper (zip/seq-zip G)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    (filter #(not (fns (zip/node %))) all-locs)))

(defn- replace-loc
  "replace the location loc1 with the location loc2, returning the root (full
  s-expression) of loc1."
  [loc1 loc2]
  (zip/root (zip/replace loc1 (zip/node loc2))))

(defn- breed
  "find a random expression in L to replace with a random expression
  in R.  replace it within L and return a new L s-expression."
  [L R]
  (let [loc1 (rand-nth (locs L))
        loc2 (rand-nth (locs R))]
    ;;(println "\nLOC1" loc1)
    ;;(println "\nLOC2" loc2)
    (replace-loc loc1 loc2)))

(defn- mutate
  [L]
  nil
  ;; from Sims' Siggraph paper:
  ;;* Any node can mutate into a new random expression. This allows
  ;;for large changes, and usually results in a fairly significant
  ;;alteration of the phenotype.
  ;;* If the node is a scalar value, it can be adjusted by the
  ;;addition of some random amount.
  ;;* If the node is a vector, it can be adjusted by adding random
  ;;amounts to each element.
  ;;* If the node is a function, it can mutate into a different
  ;;function. For example (abs X) might become (cos X). If this
  ;;mutation occurs, the arguments of the function are also adjusted
  ;;if necessary to the correct number and types.
  ;;* An expression can become the argument to a new random
  ;;function. Other arguments are generated at random if
  ;;necessary. For example X might become (* X .3).
  ;;* An argument to a function can jump out and become the new value
  ;;for that node. For example (* X .3) might become X. This is the
  ;;inverse of the previous type of mutation.
  ;;* Finally, a node can become a copy of another node from the
  ;;parent expression. For example (+ (abs X) (* Y .6)) might
  ;;become (+ (abs (* Y .6)) (* Y .6)). This causes effects similar to
  ;;those caused by mating an expression with itself. It allows for
  ;;sub-expressions to duplicate themselves within the overall
  ;;expression.
)

(defn- good-random-code?
  "does it have at least one paren?"
  [code]
  (let [code-str (str code)]
    (and (>= (count (filter #(= % \( ) code-str)) 1))))

(defn- good-colors?
  "are the values of a single color component too low or not different
  enough?"
  [colors]
  (let [min-v (apply min colors)
        max-v (apply max colors)]
    ;;(println (format "%x %x" min-v max-v))
    (and (> max-v min-color-value)
         (> (- max-v min-v) min-color-difference))))

(defn- third [x] (nth x 2))

(defn- good-image?
  "is the image not a constant color?"
  [img]
  (let [values (map #(vector
                      (bit-shift-right (bit-and % 0x00ff0000) 16)
                      (bit-shift-right (bit-and % 0x0000ff00) 8)
                      (bit-shift-right (bit-and % 0x000000ff) 0))
                    (for [x (range test-size) y (range test-size)]
                      (.getRGB img x y)))]
    (or (good-colors? (map first values))
        (good-colors? (map second values))
        (good-colors? (map third values)))))

(defn- get-good-code*
  "given a code-creator-fn, get some code that creates non-boring images."
  [code-creator-fn]
  (let [cur-count (atom 0)
        good-image (atom false)
        good-code (atom nil)]
    (while (and (< @cur-count 100)
                (not @good-image))
      (swap! cur-count inc)
      (try
        (let [cur-code (code-creator-fn)
              ;;_ (println "\n??" cur-code)
              _ (when (not (good-random-code? cur-code))
                  (throw (Exception. "badly created code"))) ;; cause exception
              img (image (eval cur-code) :size test-size)]
          ;; no exception
          (if (good-image? img)
            (do
              (reset! good-image true)
              (reset! good-code cur-code))
            (println @cur-count "boring image")))
        (catch Exception e
          (println @cur-count "code exception"))
        (catch java.util.concurrent.ExecutionException e
          (println @cur-count "execution exception"))))
    @good-code))

(defn- get-random-code
  "get a good image-creation code created randomly"
  []
  (get-good-code* (fn [] (random-code max-code-depth))))

(defn- write-png
  "write the-image to filename (which should have a .png suffix)"
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

(defn- make-random-code-and-png
  "make random code and save as files. return the code, clj-filename and png-filename"
  [timestamp suffix]
  (let [my-code (get-random-code)
        png-filename (get-png-filename timestamp suffix)
        my-image (image (eval my-code) :size full-size)
        clj-filename (get-clj-filename timestamp suffix)
        clj-basename (get-clj-basename timestamp suffix)]
    (write-png png-filename my-image)
    (spit clj-filename (str my-code))
    [my-code clj-basename png-filename]))

(defn- post-to-twitter
  [status-text the-image-filename]
  (try
    (if false
      (println "NOT posting to twitter" status-text)
      (tw/statuses-update-with-media
       :oauth-creds my-creds
       :body [(tw-req/file-body-part the-image-filename)
              (tw-req/status-body-part status-text)]))
    (catch Exception e
      ;; FIXME -- why does this always have a remote-closed exception?
      (println "caught twitter exception" e)))
  (println "waiting for a bit...")
  (Thread/sleep 5000))

(defn- append-to-gist
  [filename content]
  (gists/edit-gist
   my-gist-archive-id
   {:auth my-gist-auth
    :files { filename { :content content }}}))
;; (append-to-gist "TEST.txt" "another test 1")

(defn- post-to-web
  [the-code clj-filename png-filename]
  (let [status-text (str clj-filename " https://gist.githubusercontent.com/rogerallen/"
                         my-gist-archive-id "/raw/" clj-filename)]
  (append-to-gist clj-filename (str the-code))
  (post-to-twitter status-text png-filename)))

(defn- score-status
  "retweets count more than favorites"
  [status]
  (+ (* 3 (:retweet_count status)) (:favorite_count status)))

(defn- sanity-check-code
  "be careful with strange code you download from the web, son.  Only
  allow fns in the set of fns we know about."
  [code-str]
  (let [code-fns (->> (re-seq #"\(.+? " code-str)
                      (map #(symbol
                             (clojure.string/replace % #"[\( ]" ""))))
        code-fn-set (set code-fns)]
    (set/subset? code-fn-set fns)))

(defn- get-parent-tweet-codes
  "return a sequence of the highest-scoring two tweets code"
  []
  (let [archive  (gists/specific-gist my-gist-archive-id)
        statuses (->> (:body (tw/statuses-user-timeline
                              :oauth-creds my-creds
                              :params {:count 12
                                       :screen-name my-screen-name}))
                      (map #(assoc % :text
                                   (clojure.string/replace (:text %) #" http.*" "")))
                      (filter #(re-matches #"\d\d\d\d\d\d_\d\d\d\d\d\d_\w.clj" (:text %)))
                      (map #(assoc % :score (score-status %)))
                      (sort-by :score)
                      (reverse)
                      (take 2)
                      (map :text)
                      (map #(-> (:files archive)
                                ((keyword %))
                                :content))
                      (filter sanity-check-code)
                      (map read-string))]
    statuses))

(defn- get-random-child
  "get a good image-creation code created via breeding two other codes"
  [code0 code1]
  (get-good-code* (fn [] (breed code0 code1))))

(defn- make-random-child-and-png
  "take 2 codes, breed them and save as files. return the code, clj-filename and png-filename"
  [code0 code1 timestamp suffix]
  (let [my-code (get-random-child code0 code1)]
    (if (nil? my-code)
      [nil nil nil]
      (let [png-filename (get-png-filename timestamp suffix)
            my-image (image (eval my-code) :size full-size)
            clj-filename (get-clj-filename timestamp suffix)
            clj-basename (get-clj-basename timestamp suffix)]
        (write-png png-filename my-image)
        (spit clj-filename (str my-code))
        [my-code clj-basename png-filename]))))

;; ======================================================================
;; Public API ===========================================================
;; ======================================================================

(defn post-random-batch-to-web
  "Post a batch of 5 random codes & images to twitter and github."
  []
  (let [timestamp-str (get-timestamp-str)]
    (dorun
     (doseq [suffix "abcde"]
       (let [[the-code clj-filename png-filename] (make-random-code-and-png timestamp-str suffix)]
         (post-to-web the-code clj-filename png-filename))))))

(defn post-children-to-web
  "Find the highest-scoring parents, post a batch of 5 codes & images
  bred from those parents to twitter and github"
  []
  (let [timestamp-str (get-timestamp-str)
        [c0 c1] (get-parent-tweet-codes)]
    (dorun
     (doseq [suffix "ABCDE"]
       (let [[the-code clj-filename png-filename] (make-random-child-and-png c0 c1 timestamp-str suffix)]
         (if (nil? the-code)
           (println "!!! suffix" suffix "unable to create image")
           (post-to-web the-code clj-filename png-filename)))))))


(comment ;; code below to test things out
  ;; generate & show a random image
  (let [c (get-random-code)
        _ (println c)]
    (show (eval c)))

  ;; Careful!
  (post-random-batch-to-web)

  ;; breed things by hand...
  (def rents (get-parent-tweet-codes))
  (def dad (first rents))
  (def mom (second rents))
  (def mom '(vfrac (offset (dot (vmod (vfrac (alpha (z (vsqrt (x vsplasma))))) [-9.5645 -4.0767 2.8253]) 2.4199) (lightness-from-rgb snoise))))
  (show (eval dad))
  (show (eval mom))
  (let [c (get-random-child dad mom)
        _ (println c)]
    (if (not (nil? c))
      (show (eval c))))

  ;; Careful!
  (post-children-to-web)

  ;; (vfrac (offset (dot (vmod (vfrac (alpha (z (vsqrt (x vsplasma))))) [-9.5645 -4.0767 2.8253]) 2.4199) (lightness-from-rgb snoise)))

  )
