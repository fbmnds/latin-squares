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


(defn max-row-len
  "returns the length of the lengthiest row of a matrix"
  [x]
  (reduce max (map count x)))

(defn min-row-len
  "returns the lengths of the shortest row of a matrix"
  [x]
  (reduce min (map count x)))

(defn fill-x
  "fills all shorter rows of a matrix with :nil, if applicable"
  [x]
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



(defn contains-:nil?
  "returns true, if the seq contains :nil"
  [x]
  (cond (= (some #{:nil} (flatten x)) :nil) true :else false))

(defn only-:nil?
  "returns true, if the seq contains only :nil"
  [x]
  (= (count (filter (fn [y] (= :nil y)) (flatten x))) (count (flatten x))))

(defn any-:nil?
  "returns true, if the seq contains any :nil"
  [x]
  (= (count (filter (fn [y] (= :nil y)) (flatten x))) 0))


(defn shift-row
  "returns the right-shifted vector, if the last element is :nil;
  nil otherwise"
  [row]
  (assert (not (nil? row)))
  (cond (or (only-:nil? row) (not= (last row) :nil)) nil
        :else (vec (concat [:nil] (butlast row)))))

(defn append-variants
  "returns a vector of matrices, which results from appending all variants
  (by right-shifting) of the right-aligned vector to the matrix;
  this applies analogously, if the input matrix is nil"
  [x v]
  (assert (or (only-:nil? v) (not= (first v) :nil))) ; right-aligned vector
  (cond (or (any-:nil? v) (only-:nil? v)) (vector (conj x v))
        :else (loop [y []
                     w v]
                (cond (nil? w) y
                      :else (recur (conj y (vec (conj x w))) (shift-row w))))))


(defn build-search-base
  "generates the vector of all matrix variants to be derived from the
  nontrivial, filled matrix"
  [x]
  (assert (not (nil? x)))
  (assert (<= 2 (count x)))
  (assert (<= 2 (min-row-len x)))
  (assert (= (min-row-len x) (max-row-len x)))
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

(defn part-rows-1
  [x dim]
  (assert (= (max-row-len x) (min-row-len x)))
  (assert (<= dim (count x)))
  (assert (<= dim (max-row-len x)))
  (loop [y []
         row 0]
    (cond (= row (count x)) y
          :else (recur
                 (conj y (vec (partition dim 1 (take dim (repeat :nil)) (nth x row))))
                 (inc row)))))

(defn part-rows
  "generates from a filled matrix the grid of all possible sub-squares
  of a given sub-dimension"
  [x dim]
  (assert (= (max-row-len x) (min-row-len x))) ; i.e. filled matrix
  (assert (<= 2 dim (count x)))
  (assert (<= 2 dim (max-row-len x)))
  (mapv #( vec (partition dim 1 (take dim (repeat :nil)) %)) x))


(defn grid-base
  "generates for the given filled matrix the vector of the grid matrices
  for all possible dimensions"
  [x]
  (assert (not (nil? x)))
  (assert (<= 2 (count x)))
  (assert (<= 2 (min-row-len x)))
  (assert (= (min-row-len x) (max-row-len x)))
  (loop [y [(part-rows x 2)]
         dim 3]
    (cond (> dim (min (count x) (min-row-len x))) y
          :else (recur (into y [(part-rows x dim)]) (inc dim)))))


(defn square-at
  "returns the square of the given dimension at position i, j from a grid
  of all possible sub-squares of the same dimension"
  [parted-x dim i j]
  (assert (= dim (count (nth (nth parted-x 0) 0))))
  ;(assert (<= 2 dim (count (nth parted-x 0))))
  (assert (<= 2 dim (count parted-x)))
  (loop [y []
         di 0]
    (cond (= di dim) y
          :else (recur (conj y (nth (nth parted-x (+ i di)) j)) (inc di)))))



;user> (mapv concat (map vector [1 2 3]) (map vector [4 5 6]))
;[(1 4) (2 5) (3 6)]
;user> (partition 3 (interleave [1 2 3 4 5] [6 7 8 9 10] [5 4 3 2 1]))
;((1 6 5) (2 7 4) (3 8 3) (4 9 2) (5 10 1))
;user> (transpose [[1 2 3 4 5] [6 7 8 9 10] [5 4 3 2 1]])
;[(1 6 5) (2 7 4) (3 8 3) (4 9 2) (5 10 1)]
(defn transpose
  "returns the transpose of the matrix.
  BUG: returns vector of lists instead vector of vectors."
  [m]
  (loop [y (map vector (nth m 0))
         i 1]
    (cond (= i (count m)) y
          :else (recur (mapv concat y (map vector (nth m i))) (inc i)))))

(defn max-row-member
  "returns the maximum number of distinct elements per rows of a matrix"
  [x]
  (reduce max (map #(count (set %)) x)))

(defn min-row-member
  "returns the minimum number of distinct elements per rows of a matrix"
  [x]
  (reduce min (map #(count (set %)) x)))

(defn latin-square?-1
  [s]
  (assert (= (count s) (max-row-len s)))
  (assert (= (min-row-len s) (max-row-len s)))
  (cond (contains-:nil? s) false
        (not= (count (set (flatten s))) (max-row-len s)) false
        (not= (count (set s)) (max-row-len s)) false
        (not= (count (set (transpose s))) (max-row-len s)) false
        (not= (min-row-member s) (max-row-member s)) false
        (not= (min-row-member (transpose s)) (max-row-member (transpose s))) false
        (< (min-row-member s) (max-row-len s)) false
        (< (min-row-member (transpose s)) (max-row-len s)) false
        :else true))

(defn latin-square?-2
  [s]
  (let [dim (max-row-len s)]
  (cond (not= (count s) dim) false
        (not= (min-row-len s) dim) false
        (contains-:nil? s) false
        (not= (count (set (flatten s))) dim) false
        (not= (count (set s)) dim) false
        (not= (count (set (transpose s))) dim) false
        (not= (min-row-member s) (max-row-member s)) false
        (not= (min-row-member (transpose s)) (max-row-member (transpose s))) false
        (< (min-row-member s) dim) false
        (< (min-row-member (transpose s)) dim) false
        :else true)))

(defn latin-square?
  "returns true, if the given matrix is a latin square"
  [s]
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


(defn count-a-grid-item
  "returns the set of latin squares for the given grid item, i.e. the set of
  latin squares of the dimension that is associated to the given grid item"
  [x]
  (assert (not (nil? x)))
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


(defn solve
  [x]
  "return the accumulated set of latin squares in the given matrix"
  (let [search-base (build-search-base (fill-x x))]
    (loop [result #{}
           i 0]
      (cond (= i 2) result
            :else (recur (conj (count-a-grid-item (nth search-base i)) result)
                         (inc i))))))


;;;;;;

(defn mark-:nil-in-col
  [x]
  (loop [y ()
         col 0]
    (cond (= col (count x)) y
          :else (recur
                 (cond (contains-:nil? (nth x col))
                       (conj y col)
                       :else y)
                 (inc col)))))

(defn mark-:nil-pos
  [parted-x]
  (loop [y ()
         row 0]
    (cond (= row (count parted-x)) (set y)
          :else (recur
                 (concat y (mark-:nil-in-col (nth parted-x row)))
                 (inc row)))))

(defn remove-:nil-col
  [parted-x-row marked-pos]
  (loop [y []
         col 0]
    (cond (= col (count parted-x-row)) (vec y)
          :else (recur (cond
                        (contains? marked-pos col) y
                        :else (concat y (nth parted-x-row col)))
                       (inc col)))))

(defn reduce-parted-x
  [parted-x]
  (loop [y []
         row 0]
    (cond (= row (count parted-x)) y
          :else (recur
                 (conj y (remove-:nil-col
                          (nth parted-x row)
                          (mark-:nil-pos parted-x)))
                 (inc row)))))
