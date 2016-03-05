(ns card-game-war.logic.three-card)

(defn ante
  "Define the cards that are up for grabs to the winner"
  [piles]
  (filter #(not (nil? %)) (mapcat #(take 4 %) piles)))

(defn war-cards
  "Defines the cards that are used to battle for a given round."
  [piles]
  (->> piles
       (map #(first (drop 3 %)))
       (filter #(not (nil? %)))))

(defn adjust-piles
  "Defines the way to modify the piles given the starting piles, winning card 
and the ante (cards to the winner)"
  [piles winning-card ante]
  (map (fn [p]
         (cond
          (nil? (first (drop 3 p))) (drop 4 p)
          (= (first (drop 3 p)) winning-card) (concat (rest (drop 4 p)) ante)
          :else (drop 4 p)))
       piles))

(def config
  {:ante ante
   :war-cards war-cards
   :adjust-piles adjust-piles})
