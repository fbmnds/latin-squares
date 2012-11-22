(fn [v]
  (letfn [
    (pad [v l]
      (map #(vec (concat
        (repeat % nil) v
        (repeat (- l % (count v)) nil)))
      (range (- l (count v) -1))))
    (combine [s]
      (reduce (fn [a b] (mapcat #(map (partial conj %) b) a)) (map vector (first s)) (rest s)))
    (pads
      ([v] (pads v (apply max (map count v))))
      ([v l] (combine (map #(pad % l) v))))
    (squares [v]
      (for [l (range 2 (inc (min (count v) (count (first v)))))
        dx (range (- (count (first v)) l -1))
        dy (range (- (count v) l -1))
        :when (get-in v [dy dx])
        :when (get-in v [(+ dy l -1) (+ dx l -1)])
        :when (get-in v [dy (+ dx l -1)])
        :when (get-in v [(+ dy l -1) dx])
        :let [s (take l (drop dy (map #(take l (drop dx %)) v)))]
        :when (every? #(every? identity %) s)
        ]
       s))
    (is-latin [s]
      (and
        (apply = (concat (map set s) (apply map #(set %&) s)))
        (= (count (first s)) (count (set (first s))))))]
    (->> v
         pads
         (mapcat squares)
         set
         (filter is-latin)
         (map (comp count first))
         frequencies)))
