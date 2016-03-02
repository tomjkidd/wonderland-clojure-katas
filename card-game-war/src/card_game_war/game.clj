(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn suit->val
  [suit]
  ({:spade 1 :club 2 :diamond 3 :heart 4} suit))

(defn rank->val
  [rank]
  ({2 2
    3 3
    4 4
    5 5
    6 6
    7 7
    8 8
    9 9
    10 10
    :jack 11
    :queen 12
    :king 13
    :ace 14} rank))

(defn split-cards
  "Will split cards into n groups, equivalent of dealing
one-at-a-time into n groups."
  [cards n]
  (apply map list (partition n cards)))

(defn deal-cards
  "Takes a 52 card deck, shuffles it, and returns dealt cards for 2 players."
  []
  (let [cards (-> cards
                  (shuffle)
                  (split-cards 2))]
    {:player1 (into [] (first cards))
     :player2 (into [] (second cards))}))

(defn suit
  [card]
  (first card))

(defn rank
  [card]
  (second card))

(defn determine-round-winner
  "Return :player1 or :player2 based on who wins.
Does not allow draw."
  [player1-card player2-card]
  (let [p1-rank-val (rank->val (rank player1-card))
        p2-rank-val (rank->val (rank player2-card))]
    (if (= p1-rank-val p2-rank-val)
      (let [p1-suit-val (suit->val (suit player1-card))
            p2-suit-val (suit->val (suit player2-card))]
        (if (>= p1-suit-val p2-suit-val)
          :player1
          :player2))
      (if (>= p1-rank-val p2-rank-val)
        :player1
        :player2))))

(defn play-round [player1-card player2-card]
  (let [winner (determine-round-winner player1-card player2-card)
        winnings [player1-card player2-card]]
    (if (= :player1 winner)
      {:player1 winnings :player2 []}
      {:player1 [] :player2 winnings})))

(defn play-game [player1-cards player2-cards]
  (loop [p1-cards player1-cards
         p2-cards player2-cards]
    (let [p1-card (first p1-cards)
          p1-rem (rest p1-cards)
          p2-card (first p2-cards)
          p2-rem (rest p2-cards)]
      (cond (and (nil? p1-card) (nil? p2-card)) :draw
            (nil? p1-card) :player2
            (nil? p2-card) :player1
            :else (let [winnings (play-round p1-card
                                             p2-card)
                        new-p1-cards (concat p1-rem (:player1 winnings))
                        new-p2-cards (concat p2-rem (:player2 winnings))]
                    (recur new-p1-cards
                           new-p2-cards))))))

