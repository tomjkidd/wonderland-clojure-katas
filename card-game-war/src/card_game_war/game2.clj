(ns card-game-war.game2)

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

(defn determine-winner
  "A reduction function that determines the winner based on two cards."
  [card1 card2]
  (let [c1-rank-val (rank->val (rank card1))
        c2-rank-val (rank->val (rank card2))]
    (if (= c1-rank-val c2-rank-val)
      (let [c1-suit-val (suit->val (suit card1))
            c2-suit-val (suit->val (suit card2))]
        (if (>= c1-suit-val c2-suit-val)
          card1
          card2))
      (if (>= c1-rank-val c2-rank-val)
        card1
        card2))))

(defn winner
  "Reduces a set of cards to the winning card."
  [cards]
  (reduce determine-winner cards))

(defn play-round
  "Play a round of war. piles represents an array of card arrays, 
representing the hands of each player in the round."
  [& piles]
  (let [ante (filter #(not (nil? %)) (map first piles))
        war-cards ante
        winning-card (winner war-cards)
        adjusted-piles (map (fn [p]
                              (cond
                               (nil? (first p)) p
                               (= (first p) winning-card) (concat (rest p) ante)
                               :else (rest p)))
                            piles)]
    adjusted-piles))

(defn play-game
  "Play a game of war. piles represents an array of card arrays,
representing the hands of each player in the game."
  [& piles]
  (loop [ps piles]
    (let [debug true
          ps-with-count (map-indexed 
                         (fn [idx itm]
                           {:index idx
                            :item itm
                            :count (count itm)})
                         ps)
          out (into #{} (->> ps-with-count
                             (filter #(= 0 (:count %)))
                             (map #(:index %))))
          ps-with-cards (->> ps-with-count
                                (filter #(not (= 0 (:count %)))))]
      (cond
       (= 1 (- (count ps) (count out)))(keyword (str "player" (+ 1 (:index (first ps-with-cards)))))
       (every? #(>= 1 (:count %)) ps-with-count) :draw
       :else (let [new-ps (apply play-round ps)
                   now-out (->> new-ps
                                (map-indexed (fn [idx itm]
                                               {:index idx
                                                :out? (nil? (first itm))}))
                                (filter #(:out? %))
                                (map #(:index %))
                                (into #{}))]
               (if debug
                 (do
                   (prn new-ps)
                   (let [in (read-line)]
                     (if (= in "q")
                       :quit
                       (recur new-ps))))
                 (recur new-ps)))))))
