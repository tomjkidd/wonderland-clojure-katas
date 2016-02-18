(ns wonderland-number.finder
  (:require [clojure.math.numeric-tower :as math]))

(defn wonderland-number []
  "A number that retains its digits when multiplied by 2,3,4,5, or 6.
The number has 6 digits."
  142857)

(defn hasAllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn find-wonderland-number
  "Use the definition to find the wonderland number."
  []
  (loop [wondernum 1]
    (if (and (= 6 (count (str wondernum)))
             (hasAllTheSameDigits? wondernum (* 2 wondernum))
             (hasAllTheSameDigits? wondernum (* 3 wondernum))
             (hasAllTheSameDigits? wondernum (* 4 wondernum))
             (hasAllTheSameDigits? wondernum (* 5 wondernum))
             (hasAllTheSameDigits? wondernum (* 6 wondernum)))
      wondernum
      (recur (inc wondernum)))))

(defn equalToCubesOfDigits? [n]
  (let [xs (map #(Integer. (str %)) (seq (set (str n))))
        sum-of-cube-of-digits (apply + (map #(math/expt % 3) xs))]
    (= n sum-of-cube-of-digits)))

(defn day4-wonderland-number-finder
  []
  (->> (range 1000)
       (filter equalToCubesOfDigits?)))
