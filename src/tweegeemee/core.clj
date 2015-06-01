(ns tweegeemee.core
  (:use [clisk live])
  (:require [twitter.api.restful :as tw]
            [twitter.oauth :as tw-oauth]
            [twitter.request :as tw-req]
            [environ.core :refer [env]]
            [clojure.zip :as zip]
            [clojure.set :as set])
  (:import [java.io File]
           [javax.imageio ImageIO]))

(def my-creds (tw-oauth/make-oauth-creds
               (env :app-consumer-key)
               (env :app-consumer-secret)
               (env :user-access-token)
               (env :user-access-secret)))

(def my-screen-name (env :screen-name))

(def test-size 16)
(def full-size 720)

;; Functions for use in creating imagery.  Get more in clisk
;;(def unary-fns #{'vsin 'vcos 'vabs 'vround 'vfloor 'vfrac
;;                 'square 'vsqrt 'sigmoid 'max-component 'min-component
;;                 'length 'normalize})
;;(def binary-fns #{'v+ 'v* 'v- 'vdivide 'vmin 'vmax})
;; aliases for tweetablity
(def U0 vsin)
(def U1 vcos)
(def U2 vabs)
(def U3 vround)
(def U4 vfloor)
(def U5 vfrac)
(def U6 square)
(def U7 vsqrt)
(def U8 sigmoid)
(def U9 max-component)
(def Ua min-component)
(def Ub length)
(def Uc normalize)
(def B0 v+)
(def B1 v*)
(def B2 v-)
(def B3 vdivide)
(def B4 vmin)
(def B5 vmax)
(def T0 pos)
(def T1 noise)
(def T2 snoise)
(def unary-fns #{'U0 'U1 'U2 'U3 'U4 'U5 'U6 'U7 'U8 'U9 'Ua 'Ub 'Uc})
(def binary-fns #{'B0 'B1 'B2 'B3 'B4 'B5})
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
  "will the code fit in a tweet? does it have at least one paren?"
  [code]
  (let [code-str (str code)]
    (and (< (count code-str) 139)
         (>= (count (filter #(= % \( ) code-str)) 1))))

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
  [file-name the-image]
  (ImageIO/write the-image "png" (File. file-name)))

(defn- get-png-file-name
  [suffix]
  (str "images/"
       (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss")
                (System/currentTimeMillis))
       "_" suffix ".png"))

(defn get-good-random-code-and-png
  [suffix]
  (let [my-code (get-good-random-code)
        my-filename (get-png-file-name suffix)
        my-image (image (eval my-code) :size full-size)] ;; AA?
     (write-png my-filename my-image)
    [my-code my-filename]))

(defn post-to-twitter
  [the-code the-image-filename]
  (prn (count (str the-code)) "-" (str the-code) "-")
  (try
    (tw/statuses-update-with-media
     :oauth-creds my-creds
     :body [(tw-req/file-body-part the-image-filename)
            (tw-req/status-body-part (str the-code))])
    (catch Exception e
      (prn "caught exception" e)))
  (prn "waiting for a bit...")
  (Thread/sleep 5000))

(defn post-batch-to-twitter
  []
  (dorun
   (doseq [f "abcde"]
     (let [[c f] (get-good-random-code-and-png f)]
       (post-to-twitter c f)))))

(comment

  ;; breed
  (let [gc0 (random-code 10)
        gc1 (random-code 10)
        gc (breed gc0 gc1)
        _ (println "\n" gc "\n")]
    (show (eval gc)))

  (show (vsin (vfloor (vfloor (v* [-0.4504424629647603] pos)))))

  ;; CLISK STUFF ==========================================

  (show (v- (v* 2 pos) [1 1]) :size 720)
  (show (eval '(v- (v* 2 pos) [1 1])) :size 720)
  (show (vabs (v- (v* 2 pos) [1 1]))
        :size 720)

  (show (offset
         (v* 0.1 (scale 0.03 vsnoise))
         (rgb-from-hsl (v+ [0 0 0.5] (scale 0.3 vsnoise))))
        :size 512)


  (def checker-pattern (checker 0 1))
  (show  (scale 0.25 (offset (v* 6 vnoise) (v* vnoise checker-pattern)) )
         :size 720)

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

  )
