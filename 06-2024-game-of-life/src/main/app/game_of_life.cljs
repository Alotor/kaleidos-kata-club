(ns app.game-of-life)

(def scenarios
  {;; R-pentomimo
   :scenario-1 #{[0 0] [1 0] [1 1] [1 -1] [2 -1]}

   ;; T to stable
   :scenario-2 #{[-1 0] [0 0] [1 0] [0 -1]}

   ;; Glider
   :scenario-3 #{[0 0] [-1 -1] [-1 -2] [0 -2] [1 -2]}

   ;; Spaceship
   :scenario-4 #{[0 0] [0 -3] [-1 -2] [3 -3] [3 -1] [1 0] [2 0] [-1 0] [-1 -1]}

   ;; Acorn
   :scenario-5 #{[-1 0] [0 0] [0 -2] [2 -1] [3 0] [4 0] [5 0]}

   ;; Glider gun
   :scenario-6 #{[7 -2] [-3 -2] [1 2] [0 0] [8 -4] [1 -2] [-1 -3] [-1 3] [-2 3] [10 1] [6 -2] [-13 0] [-2 -3] [10 -5] [-14 0] [7 -3] [-4 1] [10 -4] [-14 -1] [-4 0] [3 0] [7 -1] [20 -2] [6 -3] [6 -1] [21 -2] [-3 2] [2 1] [21 -3] [8 0] [2 -1] [10 0] [-13 -1] [-4 -1] [2 0] [20 -3]}

   ;; Puffer train
   :scenario-7 #{[-7 -3] [0 0] [8 -4] [1 -2] [-4 -2] [9 0] [7 -3] [10 -4] [10 -2] [10 -3] [3 -2] [4 -1] [-5 -4] [-4 -3] [2 -2] [-4 -4] [10 -1] [-6 -4] [1 -1] [9 -4] [-4 -1] [-5 0]}

   ;; Penta-decathlon
   :scenario-8 #{[0 8] [-1 7] [0 -3] [1 -2] [0 7] [0 6] [-1 6] [-1 -2] [2 6] [1 7] [1 6] [2 -1] [1 -1] [-2 6] [-2 -1] [0 -2] [-1 -1] [0 -1]}
   })

(defn new-world
  ([]
   (new-world nil))
  ([scenario]
   ;; This set stores the cells that are alive
   (get scenarios scenario #{})))

(defn alive?
  ([world [x y]]
   (alive? world x y))
  ([world x y]
   (contains? world [x y])))

(defn dead?
  ([world [x y]]
   (dead? world x y))
  ([world x y]
   (not (alive? world x y))))

(defn neighbours
  ([world [x y]]
   (neighbours world x y))
  ([_ x y]
   [[x (inc y)]
    [(inc x) (inc y)]
    [(inc x) y]
    [(inc x) (dec y)]
    [x (dec y)]
    [(dec x) (dec y)]
    [(dec x) y]
    [(dec x) (inc y)]]))


;; Any live cell with fewer than two live neighbours dies, as if by underpopulation.
;; Any live cell with two or three live neighbours lives on to the next generation.
;; Any live cell with more than three live neighbours dies, as if by overpopulation.
;; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

(defn process-cell
  ([world [x y]]
   (process-cell world x y))
  ([world x y]
   (let [nbs (->> (neighbours world x y)
                  (filter (partial alive? world))
                  (count))
         alive? (alive? world x y)]
     (cond
       ;; Any live cell with fewer than two live neighbours dies, as if by underpopulation.
       (and alive? (< nbs 2))
       :dead

       ;; Any live cell with two or three live neighbours lives on to the next generation.
       (and alive? (or (= nbs 2) (= nbs 3)))
       :alive

       ;; Any live cell with more than three live neighbours dies, as if by overpopulation.
       (and alive? (> nbs 3))
       :dead

       ;; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
       (and (not alive?) (= nbs 3))
       :alive

       ;; Any other case (basically its already dead)
       :else
       :dead))))

(defn make-alive
  ([world [x y]]
   (make-alive world x y))
  ([world x y]
   (conj world [x y])))

(defn make-dead
  ([world [x y]]
   (make-dead world x y))
  ([world x y]
   (disj world [x y])))


(defn process-world
  [world]
  ;; We need to process alive cells and the neighbours of alive cells
  (->> (into world (mapcat (partial neighbours world) world))
       (reduce (fn [new-world cell]
                 (let [state (process-cell world cell)]
                   (case state
                     :alive (make-alive new-world cell)
                     :dead  (make-dead new-world cell))))
               (new-world))))

(defn walk-cells
  [world]
  (seq world))
