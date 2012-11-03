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

(defn contains-:nil?
  [x]
  (cond (= (some #{:nil} x) :nil) true :else false))

(defn part-rows
  [x dim]
  (assert (= (max-row-len x) (min-row-len x)))
  (assert (< dim (count x)))
  (assert (< dim (max-row-len x)))
  (loop [y []
         row 0]
    (cond (= row (count x)) y
          :else (recur
                 (conj y (vec (partition dim 1 (take dim (repeat :nil)) (nth x row))))
                 (inc row)))))

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
