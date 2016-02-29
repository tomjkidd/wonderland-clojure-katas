(ns tiny-maze.solver)

(defn find-location-of
  "Finds the first occurence of target in the maze. Returns a hash map with
:row and :col keys that have 0-indexed values. :row and :col are each nil when
the target is not found."
  [target maze]
  (let [row (->> maze
                 (map-indexed (fn [idx itm]
                                {:index idx
                                 :columns itm
                                 :contains-target? (not (= -1 (.indexOf itm target)))}))
                 
                 (filter #(:contains-target? %))
                 (first))
        col (->> (:columns row)
                 (map-indexed (fn [idx itm]
                                {:index idx
                                 :is-target? (= itm target)}))
                 (filter #(:is-target? %))
                 (first))]
    {:row (:index row)
     :col (:index col)}))

(defn find-start
  [maze]
  (find-location-of :S maze))

(defn valid-location?
  "Determines if the given location is valid (within the bounds) for the 
current maze."
  [maze loc]
  (not (nil? (get-in maze [(:row loc) (:col loc)]))))

(defn previously-visited?
  "Determines if the given location has already been visited."
  [maze loc]
  (= :x (get-in maze [(:row loc) (:col loc)])))

(defn allowed-move?
  "Determines if the given location is an allowed move."
  [maze loc]
  (let [symbol (get-in maze [(:row loc) (:col loc)])]
    (or (= 0 symbol)
        (= :E symbol))))

(defn get-moves
  "Lists the available moves for a given maze and location"
  [maze loc]
  (let [cur-row (:row loc)
        cur-col (:col loc)
        up (assoc loc :row (- cur-row 1))
        down (assoc loc :row (+ cur-row 1))
        left (assoc loc :col (- cur-col 1))
        right (assoc loc :col (+ cur-col 1))]
    (filter (fn [loc]
              (and (valid-location? maze loc)
                   (not (previously-visited? maze loc))
                   (allowed-move? maze loc)))
            [up down left right])))

(defn update-maze
  "Responsible for the text display of the current state of the maze."
  [maze loc value]
  (assoc-in maze [(:row loc) (:col loc)] value))

(declare solve-maze-helper-moves)

(defn solve-maze-helper-move [cur-maze move]
  (if (= move (find-location-of :E cur-maze))
    (update-maze cur-maze move :x)
    (do (let [moves (get-moves cur-maze move)]
          (if (= 0 (count moves))
            nil
            (solve-maze-helper-moves (update-maze cur-maze move :x) moves))))))

(defn solve-maze-helper-moves [cur moves]
  (->> moves
       (map #(solve-maze-helper-move cur %))
       (filter #(not (nil? %)))
       (first)))

(defn solve-maze [maze]
  (let [start (find-start maze)
        init (update-maze maze start :x)]
    (solve-maze-helper-moves init (get-moves init start))))
