(def v1 '[[A B C D]
         [A C D B]
         [B A D C]
         [D C A B]])


(def v2 '[[A B C D E F]
         [B C D E F A]
         [C D E F A B]
         [D E F A B C]
         [E F A B C D]
         [F A B C D E]])

(def v3 '[[A B C D]
         [B A D C]
         [D C B A]
         [C D A B]])

(def v4 '[[B D A C B]
         [D A B C A]
         [A B C A B]
         [B C A B C]
         [A D B C A]])

(def v5 [  [2 4 6 3]
        [3 4 6 2]
          [6 2 4]  ])

(def v6 [[1]
        [1 2 1 2]
        [2 1 2 1]
        [1 2 1 2]
        []       ])

(def v7 [[3 1 2]
        [1 2 3 1 3 4]
        [2 3 1 3]    ])

(def v8 [[8 6 7 3 2 5 1 4]
        [6 8 3 7]
        [7 3 8 6]
        [3 7 6 8 1 4 5 2]
              [1 8 5 2 4]
              [8 1 2 4 5]])


(letfn [
(mapv
  ([f coll]
     (-> (reduce (fn [v o] (conj! v (f o))) (transient []) coll)
         persistent!))
  ([f c1 c2]
     (into [] (map f c1 c2)))
  ([f c1 c2 c3]
     (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
     (into [] (apply map f c1 c2 c3 colls))))

(max-row-len [x]
  (reduce max (map count x)))

(min-row-len [x]
  (reduce min (map count x)))


(fill-x  [x]
  (let [dim (max-row-len x)
        y (mapv vector x (map #(- dim (count %)) x))]
    (mapv (fn [x] (vec (concat (repeat (x 1) :nil) (x 0)))) y)))


(vec-contains-:nil? [v]
  (cond (some #(= % :nil) v) true :else false))


(only-:nil? [v]
  (cond (some #(not= % :nil) v) false :else true))


(any-:nil? [x]
  (not (vec-contains-:nil? (flatten x))))


(shift-left  [row]
   (cond (or (only-:nil? row) (not= (row 0) :nil)) nil
        :else (vec (concat (rest row) [:nil]))))



(append-variants  [x v]
   (cond (or (any-:nil? v) (only-:nil? v)) (conj [] (vec (conj x v)))
        :else (loop [y []
                     w v]
                (cond (nil? w) y
                      :else (recur (conj y (vec (conj x w))) (shift-left w))))))


(build-search-base  [x]
   (cond (any-:nil? x) (vector x)
        :else (loop [y (append-variants nil (x 0))
                     row-x 1]
                (cond (= row-x (count x)) y
                      :else (recur
                             (loop [iter-y 0
                                    new-y []]
                               (cond (= iter-y (count y)) new-y
                               :else (recur
                                      (inc iter-y)
                                      (into new-y
                                            (append-variants (y iter-y)
                                                             (x row-x))))))
                             (inc row-x))))))



(part-rows  [x dim]
   (mapv #( vec (partition dim 1 (take dim (repeat :nil)) %)) x))


(build-grid-base [x FIX]
   (loop [y [(part-rows x 2)]
         dim 3]
    (cond (> dim (min (count x) FIX)) y
          :else (recur (into y [(part-rows x dim)]) (inc dim)))))


(square-at  [parted-x dim i j]
   (loop [y []
         di 0]
    (cond (= di dim) y
          :else (let [curr-row (nth (nth parted-x (+ i di)) j)]
                  (cond (vec-contains-:nil? curr-row) nil
                        :else (recur (conj y curr-row) (inc di)))))))


(latin-square?  [s]
  (cond (nil? s) false
        :else (and
               (apply = (concat (map set s) (apply map #(set %&) s)))
               (= (count (first s)) (count (set (first s)))))))


(count-a-grid-item  [x]
  (let [dim-sq (count (nth (nth x 0) 0))
        dim-x-x (inc (- (count x) dim-sq))
        dim-x-y (count (nth x 0))]
    (for [i (range dim-x-x)
          j (range dim-x-y)
          :let [sq (square-at x dim-sq i j)]]
      (cond (latin-square? sq) sq))))


(solve [x]
  (let [search-base (build-search-base (fill-x x))
        dim-base (count search-base)
        FIX (min-row-len x)]
    (for [i (range dim-base)
          :let [grid-base (build-grid-base (nth search-base i) FIX)]
          j (range (count grid-base))
          :let [s (count-a-grid-item
                   (nth grid-base j))]]
      s)))


(summary  [lsq]
  (dissoc
   (reduce conj {}
           (for [[x xs]
                 (group-by identity
                           (sort < (for [r lsq]
                                     (count r))))]
             [x (count xs)])) 0))]

(fn [x]  (summary (reduce into #{} (solve x)))))
