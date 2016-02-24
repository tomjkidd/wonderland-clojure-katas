(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(def more-words (-> "morewords.edn"
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

(defn choose-best-word
  [target candidates]
  (let [tmps (map (fn [wd] {:word wd :diffs (count-diffs wd target)}) candidates)
        find-best (fn [min cur]
                   (if (nil? min)
                     cur
                     (if (< (:diffs min) (:diffs cur))
                       min
                       cur)))]
    (:word (reduce find-best nil tmps))))

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
              next (choose-best-word trg candidates)]
          (recur (conj acc next) next trg))))))

(defn for-fun
  []
  (let [pairs [["jail" "free"]
               ["love" "baby"]
               ["crap" "hope"]]]
    (map #(find-doublets more-words (first %) (second %)) pairs)))

(defn doublets
  [word1 word2]
  (if (not (same-length? word1 word2))
    []
    (-> (word-pool words word1)
        (find-doublets word1 word2))))
