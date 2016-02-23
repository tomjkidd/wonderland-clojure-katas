(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn same-length?
  [w1 w2]
  (= (count w1) (count w2)))

(defn word-pool
  [words target]
  (->> words
       (filter #(same-length? target %))))

(defn count-diffs
  [word1 word2]
  (reduce (fn [acc cur]
            (if cur (+ acc 1) acc))
          0
          (map (complement =) word1 word2)))

(defn with-n-diffs
  "Looks through pool for words with n position-based differences from word."
  [n pool word]
  (->> pool 
       (map (fn [itm] [itm (count-diffs itm word)]))
       (filter #(= n (second %)))
       (map first)))

(defn find-doublets
  [word-pool
   source
   target]
  (loop [acc [source] cur source trg target]
    (if (or (= cur trg) (nil? cur))
      (if (nil? cur) [] acc)
      (do
        (let [one-diff (with-n-diffs 1 word-pool cur)
              candidates (filter #(not (contains? (set acc) %)) one-diff)
              next (first (reduce (fn [min cur]
                                    (if (nil? min)
                                      cur
                                      (if (< (second min) (second cur))
                                        min
                                        cur)))
                                  nil
                                  (map (fn [wd] [wd (count-diffs wd trg)]) candidates)))]
          (recur (conj acc next) next trg))))))

(defn doublets
  [word1 word2]
  (if (not (same-length? word1 word2))
    []
    (-> (word-pool words word1)
        (find-doublets word1 word2))))
