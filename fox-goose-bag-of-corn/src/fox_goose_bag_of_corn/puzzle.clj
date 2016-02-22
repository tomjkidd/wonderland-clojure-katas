(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def plan
  [[[:fox :goose :corn :you] [:boat] []]
   [[:fox :corn] [:boat :you :goose] []]
   [[:fox :corn] [:boat] [:you :goose]]
   [[:fox :corn] [:boat :you] [:goose]]
   [[:fox :corn :you] [:boat] [:goose]]
   [[:corn] [:boat :you :fox] [:goose]]
   [[:corn] [:boat] [:you :fox :goose]]
   [[:corn] [:boat :you :goose] [:fox]]
   [[:corn :you :goose] [:boat] [:fox]]
   [[:goose] [:boat :you :corn] [:fox]]
   [[:goose] [:boat] [:you :fox :corn]]
   [[:goose] [:boat :you] [:fox :corn]]
   [[:goose :you] [:boat] [:fox :corn]]
   [[] [:boat :you :goose] [:fox :corn]]
   [[] [:boat] [:you :goose :fox :corn]]])

;; TODO: How could I generate the solution's moves?

(def moves
  "Here we say what is moved where.
  It is implied that each has to pass through the boat. 
  Nil means that the person moves without any cargo."
  [[:goose :right]
   [nil :left]
   [:fox :right]
   [:goose :left]
   [:corn :right]
   [nil :left]
   [:goose :right]])

(defn index->loc
  [ind]
  ({0 :left 1 :boat 2 :right} ind))

(defn loc->index
  [loc]
  ({:left 0 :boat 1 :right 2} loc))

(defn locate
  [current-position
   target]
  (->> current-position
       (map #(contains? (set %) target))
       (map-indexed (fn [idx itm] [idx itm]))
       (filter #(second %))
       (first)
       (first)
       (index->loc)))

(defn move-cargo-to
  [[cur-left cur-boat cur-right :as cur]
   cargo
   to-loc]
  (let [to-move (if (nil? cargo) [:you] [:you cargo])
        from-loc (locate cur :you)
        from-idx (loc->index from-loc)
        from-set (apply disj (set (nth cur from-idx)) to-move)
        to-idx (loc->index to-loc)
        to-set (apply conj (set (nth cur to-idx)) to-move)]
    (-> cur
        (assoc from-idx (into [] from-set))
        (assoc to-idx (into [] to-set)))))

(defn apply-moves
  [start-pos moves]
  (loop [acc [start-pos] remaining moves]
    (if (empty? remaining)
      acc
      (let [cur-pos (last acc)
            [cargo loc] (first remaining)
            boat-move (move-cargo-to cur-pos cargo :boat)
            next-pos (move-cargo-to cur-pos cargo loc)]
        (recur (conj acc boat-move next-pos) (rest remaining))))))

(defn river-crossing-plan []
  (apply-moves (first start-pos) moves))
