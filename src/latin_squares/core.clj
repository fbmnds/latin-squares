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


(defn max-row-len [x] (reduce max (map count x)))
(defn min-row-len [x] (reduce min (map count x)))

(defn fill-x
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
  [x dim] ; dim counted from 0
  (assert (= (max-row-len x) (min-row-len x)))
  (assert (<= 2 dim (count x)))
  (assert (<= 2 dim (max-row-len x)))
  (mapv #( vec (partition dim 1 (take dim (repeat :nil)) %)) x))


(defn square-at
  [parted-x dim i j]
  (assert (= dim (count (nth (nth parted-x 0) 0))))
  (assert (<= 2 dim (count (nth parted-x 0))))
  (assert (<= 2 dim (count parted-x)))
  (loop [y []
         di 0]
    (cond (= di dim) y
          :else (recur (conj y (nth (nth parted-x (+ i di)) j)) (inc di)))))


(defn contains-:nil?
  [x]
  (cond (= (some #{:nil} (flatten x)) :nil) true :else false))


;user> (mapv concat (map vector [1 2 3]) (map vector [4 5 6]))
;[(1 4) (2 5) (3 6)]
;user> (partition 3 (interleave [1 2 3 4 5] [6 7 8 9 10] [5 4 3 2 1]))
;((1 6 5) (2 7 4) (3 8 3) (4 9 2) (5 10 1))
;user> (transpose [[1 2 3 4 5] [6 7 8 9 10] [5 4 3 2 1]])
;[(1 6 5) (2 7 4) (3 8 3) (4 9 2) (5 10 1)]
(defn transpose
  [m]
  (loop [y (map vector (nth m 0))
         i 1]
    (cond (= i (count m)) y
          :else (recur (mapv concat y (map vector (nth m i))) (inc i)))))

(defn max-row-member [x] (reduce max (map #(count (set %)) x)))
(defn min-row-member [x] (reduce min (map #(count (set %)) x)))

(defn latin-square?
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
