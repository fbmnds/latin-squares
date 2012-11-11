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


(fn [x](letfn [
(max-row-len [x]
  (reduce max (map count x)))

(min-row-len [x]
  (reduce min (map count x)))

(fill-x  [x]
  (loop [y []
         row 0]
    (cond (= row (count x)) y
          :else (recur
                 (cond (< (count (nth x row)) (max-row-len x))
                       (conj y
                             (vec
                              (concat (nth x row)
                                      (take (- (max-row-len x)
                                               (count (nth x row))) (repeat :nil)))))
                       :else (conj y (nth x row)))
                 (inc row)))))



(contains-:nil?  [x]
  (cond (= (some #{:nil} (flatten x)) :nil) true :else false))

(only-:nil? [x]
  (= (count (filter (fn [y] (= :nil y)) (flatten x))) (count (flatten x))))

(any-:nil? [x]
  (= (count (filter (fn [y] (= :nil y)) (flatten x))) 0))


(shift-row  [row]
   (cond (or (only-:nil? row) (not= (last row) :nil)) nil
        :else (vec (concat [:nil] (butlast row)))))

(append-variants  [x v]
   (cond (or (any-:nil? v) (only-:nil? v)) (conj [] (vec (conj x v)))
        :else (loop [y []
                     w v]
                (cond (nil? w) y
                      :else (recur (conj y (vec (conj x w))) (shift-row w))))))


(build-search-base  [x]
   (cond (any-:nil? x) (vector x)
        :else (loop [y (append-variants nil (nth x 0))
                     row-x 1]
                (cond (= row-x (count x)) y
                      :else (recur
                             (loop [iter-y 0
                                    new-y []]
                               (cond (= iter-y (count y)) new-y
                               :else (recur
                                      (inc iter-y)
                                      (into new-y
                                            (append-variants (nth y iter-y)
                                                             (nth x row-x))))))
                             (inc row-x))))))



(part-rows  [x dim]
   (mapv #( vec (partition dim 1 (take dim (repeat :nil)) %)) x))


(build-grid-base [x]
   (loop [y [(part-rows x 2)]
         dim 3]
    (cond (> dim (min (count x) (min-row-len x))) y
          :else (recur (into y [(part-rows x dim)]) (inc dim)))))


(square-at  [parted-x dim i j]
   (loop [y []
         di 0]
    (cond (= di dim) y
          :else (recur (conj y (nth (nth parted-x (+ i di)) j)) (inc di)))))

(transpose  [m]
  (loop [y (map vector (nth m 0))
         i 1]
    (cond (= i (count m)) y
          :else (recur (mapv concat y (map vector (nth m i))) (inc i)))))

(max-row-member  [x]
  (reduce max (map #(count (set %)) x)))

(min-row-member  [x]
  (reduce min (map #(count (set %)) x)))


(latin-square?  [s]
  (let [dim (max-row-len s)]
    (cond (and (not (contains-:nil? s))
               (= dim
                  (count s)
                  (min-row-len s)
                  (count (set (flatten s)))
                  (count (set s))
                  (count (set (transpose s)))
                  (min-row-member s)
                  (max-row-member s)
                  (min-row-member (transpose s))
                  (max-row-member (transpose s))))
          true
          :else false)))


(count-a-grid-item  [x]
   (let [dim-sq (count (nth (nth x 0) 0))
        dim-x-x (inc (- (count x) dim-sq))
        dim-x-y (count (nth x 0))]
    (loop [sq (square-at x dim-sq 0 0)
           result (cond (latin-square? sq) (conj #{} sq)
                        :else #{})
           i 0
           j 1]
      (cond (= i dim-x-x) result
            :else (recur (cond (and (< i dim-x-x) (< j dim-x-y))
                               (square-at x dim-sq i j)
                               :else sq)
                         (cond (and (< i dim-x-x) (< j dim-x-y))
                               (cond (latin-square? sq) (conj result sq)
                                     :else result)
                               :else result)
                         (cond (= j dim-x-y) (inc i)
                               :else i)
                         (cond (= j dim-x-y) 0
                               :else (inc j)))))))



(solve  [x]
  (let [search-base (build-search-base (fill-x x))
        dim-base (count search-base)
        result (atom [])]
    (doseq [i (range dim-base)]
      (let [grid-base (build-grid-base (nth search-base i))]
        (doseq [j (range (count grid-base))]
          (swap!  result into (count-a-grid-item (nth grid-base j))))))
    (set @result)))


(summary  [lsq]
  (reduce conj {} (for [[x xs]
                        (group-by identity
                                  (sort < (for [r lsq]
                                    (count r))))]
                    [x (count xs)])))]
 (summary (solve x))))
