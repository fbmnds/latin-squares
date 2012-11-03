;'(1 2 3)

(defn min-1
  [x & more]
  (loop [min x
         more (seq more)]
    (if-let [i (first  more)]
      (recur (if (> i min) min i) (next more))
      min)))

(defn min-2
  [x & more]
  (loop [min x
         [i & more] (seq more)]
    (if i
      (recur (if (> i min) min i) more)
      min)))

(defn min-2a
  [x & more]
  (loop [min x
         [i & more] (seq more)]
    (if i
      (recur (if (< i min) i min) more)
      min)))

(defn min-3
  [x & more]
  (reduce (fn [i j] (if (< i j) i j)) x more))

(defn min-3a
  [& more]
  (reduce (fn [i j] (if (< i j) i j)) more))

(defn zipm-1
  [ks vs]
  (loop [m {}
         ks (seq ks)
         vs (seq vs)]
    (if (and (first ks) (first vs))
      (recur (assoc m (first ks) (first vs))  (next ks) (next vs))
      m)))

(defn zipm-2
  [keys vals]
  (loop [m {}
         [ks & keys] (seq keys)
         [vs & vals] (seq vals)]
    (if (and ks vs)
      (recur (assoc m ks vs) keys vals)
      m)))

(defn zipm-3
  [keys vals]
  (reduce (fn [result-of-the-prev-step [ks vs]]
            (assoc result-of-the-prev-step ks vs))
          {} (map vector keys vals) ))

(defn zipm-4
  [keys vals]
  (apply hash-map (interleave keys vals)))

(defn zipm-5
  [keys vals]
  (into {} (map vector keys vals)))

;; SCIP

(defn abs
  [x]
  (cond (< x 0) (- x)
        :else x))

(defn <eps?
  [x]
  (cond (< x 0.001) true
        :else false))

(defn average
  [& xs]
  (/ (apply + xs) (count xs)))

(defn sq
  [x]
  (* x x))

(defn sqrt
  [x]
  (defn precision?
    [guess x]
    (cond (<eps? (abs (- (sq guess) x))) true
          :else false))
  (defn try-guess
    [guess x]
    (cond (precision? guess x) guess
          :else (try-guess (average guess (/ x guess)) x)))
  (cond (< x 0) (println "error: sqrt[x < 0]")
        (<eps? x) 0
        (<eps? (abs (- x 1))) 1
        :else (try-guess 1.0 x)))

;; http://blog.malcolmsparks.com/?p=17

(defn
  ^{:doc "Apply functions to values in a map."}
  mapply
  [m & kfs]
  (apply hash-map
         (mapcat (fn [[k f]] [k (f (get m k))])
                 (partition 2 kfs))))

;; Increment :count and retain the value of :foo.
(mapply {:count 1 :foo "foo" :a 1} :count inc :foo identity :a identity)

;; Remove :count and set the :foo to "bar".
(mapply {:count 1 :foo "foo"} :foo (constantly "bar"))

;;;;;;;;;;;;;;;;;;;;;;;;

(fn [coll]
  (cond (= (empty coll) (empty {:a 1})) :map
        (= (empty coll) (empty '(1))) :list
        (= (empty coll) (empty #{1})) :set
        (= (empty coll) (empty [1])) :vector
        :else :unknown))

(defn t [coll]
  (cond (= (empty coll) (empty {:a 1})) :map
        (= (empty coll) (empty [])) (cond (= (peek coll) (first coll)) :list
                                           :else :vector)
        (= (empty coll) (empty #{1})) :set
        :else :unknown))

(fn [coll]
  (cond (= (clojure.core/str coll) "()") :list
        (= (clojure.core/str coll) "[]") :vector
        (= (clojure.core/str coll) "#{}") :set
        (= (clojure.core/str coll) "{}") :map
        (= (empty coll) (empty {:a 1})) :map
        (= (empty coll) ())
           (cond (= (first (conj coll :test)) :test) :list
                 :else :vector)
        (= (empty coll) (empty #{1})) :set
        :else :unknown))
