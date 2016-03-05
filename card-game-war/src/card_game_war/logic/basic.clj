(ns card-game-war.logic.basic)

(defn ante
  "Define the cards that are up for grabs to the winner"
  [piles]
  (filter #(not (nil? %)) (map first piles)))

(def war-cards
  "Defines the cards that are used to battle for a given round."
  ante)

(defn adjust-piles
  "Defines the way to modify the piles given the starting piles, winning card 
and the ante (cards to the winner)"
  [piles winning-card ante]
  (map (fn [p]
         (cond
          (nil? (first p)) p
          (= (first p) winning-card) (concat (rest p) ante)
          :else (rest p)))
       piles))

(def core
  {:ante ante
   :war-cards war-cards
   :adjust-piles adjust-piles})
