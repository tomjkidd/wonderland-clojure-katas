(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game2 :refer :all]))

(deftest test-winner
  (testing "the highest rank wins the cards in the round"
    (is (= (winner [[:spade 2] [:spade 3]])
           [:spade 3]))
    (is (every? #(= true %)
                (map #(= (winner [[:spade (first %)] [:spade (second %)]])
                         [:spade (second %)])
                     [[2 3]
                      [3 4]
                      [4 5]
                      [5 6]
                      [6 7]
                      [7 8]
                      [8 9]
                      [9 10]
                      [10 :jack]
                      [:jack :queen]
                      [:queen :king]
                      [:king :ace]]))))
  (testing "queens are higher rank than jacks"
    (is (= (winner [[:spade :queen] [:spade :jack]])
           [:spade :queen])))
  (testing "kings are higher rank than queens"
    (is (= (winner [[:spade :king] [:spade :queen]])
           [:spade :king])))
  (testing "aces are higher rank than kings"
    (is (= (winner [[:spade :ace] [:spade :king]])
           [:spade :ace])))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= (winner [[:club 2] [:spade 2]])
           [:club 2])))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= (winner [[:diamond 2] [:club 2]])
           [:diamond 2])))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= (winner [[:heart 2] [:diamond 2]])
           [:heart 2]))))

(deftest test-play-game
  (testing "the player loses when they run out of cards")
  (testing "player wins with better card (depends on how the def cards is defined!)"
    (is (= (apply play-game (map #(take 2 %) (split-cards cards 2)))
           :player2))
    (is (= (apply play-game (map #(take 2 %) (reverse (split-cards cards 2))))
           :player1))))

