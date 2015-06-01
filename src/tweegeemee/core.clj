(ns tweegeemee.core
  (:use [clisk live])
  (:require [twitter.api.restful :as tw]
            [twitter.oauth :as tw-oauth]
            [twitter.request :as tw-req]
            [environ.core :refer [env]]
            [clojure.zip :as zip]
            [clojure.set :as set]))

(def my-creds (tw-oauth/make-oauth-creds
               (env :app-consumer-key)
               (env :app-consumer-secret)
               (env :user-access-token)
               (env :user-access-secret)))

(def my-screen-name (env :screen-name))

(def unary-fns #{'vsin 'vcos 'vabs 'vround 'vfloor 'vfrac
                 'square 'vsqrt 'sigmoid 'max-component 'min-component
                 'length 'normalize})
(def binary-fns #{'v+ 'v* 'v- 'vdivide 'vmin 'vmax})
;; todo (constrain stuff)
;; vpow rotate scale offset matrix 'dot 'cross3 'vmod
;; gradient ?
;; clamp (3 args)
;; theta radius
(def fns (set/union unary-fns binary-fns))

(defn random-fn [n]
  (if (= n 2)
    (rand-nth (seq binary-fns))
    (rand-nth (seq unary-fns))))
;;(random-fn)

(defn rand-value []
  (let [x (- (rand 2) 1)
        x (/ (Math/floor (* x 1000)) 1000.0)]
    x))

(defn random-terminal []
  (let [r (rand)]
    (if (< r 0.10)
      'pos
      (if (< r 0.15)
        'noise
      (if (< r 0.2)
        'snoise
        (if (< r 0.5)
          (rand-value)
          (if (< r 0.75)
            [(rand-value) (rand-value)]
            [(rand-value) (rand-value) (rand-value)])))))))
;;(random-terminal)

(defn random-code
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

(defn locs [G]
  (let [zipper (zip/seq-zip G)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    (filter #(not (fns (zip/node %))) all-locs)))

(defn replace-loc [l r]
  (zip/root (zip/replace l (zip/node r))))

(defn breed [L R]
  (let [l (rand-nth (locs L))
        r (rand-nth (locs R))]
    (replace-loc l r)))

(comment
  ;; random
    (try
      (let [gc (random-code 10)
            gce (eval gc)
            _ (show gce :size 256)
            _ (println "\n" gc "\n")]
        gc)
      (catch Exception e
        (prn "caught error" e)))

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
