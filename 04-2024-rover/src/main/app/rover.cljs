
(ns app.rover)

(def NORTH ::north)
(def EAST ::east)
(def SOUTH ::south)
(def WEST ::west)

(defn init
  [xpos ypos direction]
  {:x xpos :y ypos :dir direction})

(defn world
  [width height obstacles]
  {:width width
   :height height
   :obstacles (set obstacles)})

(def x :x)
(def y :y)
(def dir :dir)

(defn fix-coord
  [{:keys [width height]} [x y]]

  (let [x' (cond
             (>= x width) (- x width)
             (< x 0) (+ x width)
             :else x)

        y' (cond
             (>= y height) (- y height)
             (< y 0) (+ y height)
             :else y)]
    [x' y']))

(defn check-obstacle
  [{:keys [obstacles]} [x y]]
  
  (when-not (contains? obstacles [x y])
    [x y]))

(defmulti execute-command (fn [_ _ cmd] cmd))

(defmethod execute-command "f"
  [world {:keys [dir x y] :as rover} _]
  (let [coord
        (case dir
          ::north [x (inc y)]
          ::east  [(inc x) y]
          ::south [x (dec y)]
          ::west  [(dec x) y])
        [x y :as coord] (fix-coord world coord)]
    (if (check-obstacle world coord)
      (assoc rover :x x :y y)
      ::abort)))

(defmethod execute-command "b"
  [world {:keys [dir x y] :as rover} _]
  (let [coord
        (case dir
          ::north [x (dec y)]
          ::east  [(dec x) y]
          ::south [x (inc y)]
          ::west  [(inc x) y])
        [x y :as coord] (fix-coord world coord)]
    (if (check-obstacle world coord)
      (assoc rover :x x :y y)
      ::abort)))

(defmethod execute-command "l"
  [world {:keys [dir] :as rover} _]
  (let [new-dir
        (case dir
          ::north ::west
          ::east  ::north
          ::south ::east
          ::west  ::south)]
    (assoc rover :dir new-dir)))

(defmethod execute-command "r"
  [world {:keys [dir] :as rover} _]
  (let [new-dir
        (case dir
          ::north ::east
          ::east  ::south
          ::south ::west
          ::west  ::north)]
    (assoc rover :dir new-dir)))

(defmethod execute-command :default
  [_ rover _]
  rover)

(defn commands
  [rover world cmds]
  (reduce
   (fn [rover cmd]
     (let [result (execute-command world rover cmd)]
       (if (= result ::abort)
         (throw (js/Error. "Obstacle detected!"))
         result)))
   rover
   cmds))
