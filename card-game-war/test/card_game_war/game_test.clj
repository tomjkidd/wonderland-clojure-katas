(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game2 :refer :all]))

(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= (winner [[:spade 2] [:spade 3]])
           [:spade 3])))
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
  (testing "the player loses when they run out of cards"))

