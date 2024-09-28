(ns app.ui
  (:require
   [app.game-of-life :as gol]))

(def SQ_BASE 10)
(def SQ_LINE_COLOR "#222222")
(def SQ_CENTER "red")
(def SQ_HOVER "yellow")
(def SQ_FILL "white")

(defonce main-canvas (.getElementById js/document "mainCanvas"))
(defonce start-button (.getElementById js/document "startBtn"))
(defonce pause-button (.getElementById js/document "pauseBtn"))
(defonce clean-button (.getElementById js/document "cleanBtn"))
(defonce zoom-in-button (.getElementById js/document "zoomInBtn"))
(defonce zoom-out-button (.getElementById js/document "zoomOutBtn"))
(defonce examples-cases-select (.getElementById js/document "examplesCasesSelect"))

(defonce dump-button (.getElementById js/document "dumpBtn"))

(defonce state
  (let [ctx (.getContext main-canvas "2d")
        width (.-innerWidth js/window)
        height (.-innerHeight js/window)]
    (set! (.-imageSmoothingEnabled ctx) false)
    (set! (.-width (.-canvas ctx)) width)
    (set! (.-height (.-canvas ctx)) height)

    (atom {:zoom 2
           :sq-size SQ_BASE
           :offset-x 0
           :offset-y 0
           :width width
           :height height
           :state :pause
           :last-process nil
           :world (gol/new-world)})))

(defn sq-size
  []
  (* (:sq-size @state) (:zoom @state)))

(defn to-grid-coords
  [client-x client-y]
  (let [x (.floor js/Math (/ (- client-x (/ (:width @state) 2) (:offset-x @state)) (sq-size)))
        y (.floor js/Math (/ (- client-y (/ (:height @state) 2) (:offset-y @state)) (sq-size)))]
    {:x x :y y}))

(defn from-grid-coords
  [cx cy]
  (let [x (+ (* cx (sq-size)) (:offset-x @state) (/ (:width @state) 2))
        y (+ (* cy (sq-size)) (:offset-y @state) (/ (:height @state) 2))]
    {:x x :y y}))

(defn render-pixel
  ([cx cy color]
   (let [ctx (.getContext main-canvas "2d")]
     (render-pixel ctx cx cy color)))
  ([ctx cx cy color]
   (let [{:keys [x y]} (from-grid-coords cx cy)]
     (.beginPath ctx)
     (set! (.-fillStyle ctx) color)
     (.rect ctx x y (sq-size) (sq-size))
     (.fill ctx))))

(defn render-grid
  []
  (let [ctx (.getContext main-canvas "2d")
        p (to-grid-coords 0 0)
        s (from-grid-coords (:x p) (:y p))]
    (.clearRect ctx 0 0 (:width @state) (:height @state))
    (set! (.-strokeStyle ctx) SQ_LINE_COLOR)

    (doseq [i (range (:x s) (inc (:width @state)) (sq-size))]
      (.beginPath ctx)
      (.moveTo ctx i 0)
      (.lineTo ctx i (:height @state))
      (.stroke ctx))

    (doseq [i (range (:y s) (inc (:height @state)) (sq-size))]
      (.beginPath ctx)
      (.moveTo ctx 0 i)
      (.lineTo ctx (:width @state) i)
      (.stroke ctx))

    (render-pixel ctx 0 0 SQ_CENTER)

    (doseq [[x y] (gol/walk-cells (:world @state))]
      (render-pixel ctx x y SQ_FILL))))

(defn handle-window-resize
  [e]
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)
        ctx (.getContext main-canvas "2d")]
    (set! (.-width (.-canvas ctx)) width)
    (set! (.-height (.-canvas ctx)) height)
    (swap! state assoc :width width :height height)
    (render-grid)))

(defn handle-canvas-move
  [e]
  (let [{:keys [x y]} (to-grid-coords (.-clientX e) (.-clientY e))]

    (cond
      (= 1 (.-which e))
      (swap! state update :world gol/make-alive x y)

      (= 2 (.-which e))
      (let [offset-x (+ (:offset-x @state) (- (.-clientX e) (:drag-start-x @state)))
            offset-y (+ (:offset-y @state) (- (.-clientY e) (:drag-start-y @state)))
            drag-start-x (.-clientX e)
            drag-start-y (.-clientY e)]
        (swap! state assoc
               :offset-x offset-x
               :offset-y offset-y
               :drag-start-x drag-start-x
               :drag-start-y drag-start-y))

      (= 3 (.-which e))
      (swap! state update :world gol/make-dead x y))
    
    (render-grid)
    (render-pixel x y SQ_HOVER)))

(defn handle-canvas-context-menu
  [e]
  (.preventDefault e))

(defn handle-canvas-mouse-down
  [e]
  (let [{:keys [x y]} (to-grid-coords (.-clientX e) (.-clientY e))]
    (case (.-which e)
      1
      (swap! state update :world gol/make-alive x y)

      2
      (swap! state assoc :drag-start-x (.-clientX e) :drag-start-y (.-clientY e))

      3
      (swap! state update :world gol/make-dead x y)
      )))

(defn main-loop
  []
  (let [last-process (or (:last-process @state) 0)
        delta (/ (- (js/Date.now) last-process) 30)]
    (when (> delta 1)
      (swap!
       state
       (fn [s]
         (-> s
             (assoc :last-process (js/Date.now))
             (cond-> (= (:state @state) :running)
               (update :world gol/process-world)))))
      (render-grid))

    (.requestAnimationFrame js/window main-loop)))

(defn init-handlers
  []
  (.addEventListener js/window "resize" handle-window-resize)
  (.addEventListener main-canvas "mousemove" handle-canvas-move)
  (.addEventListener main-canvas "contextmenu" handle-canvas-context-menu)
  (.addEventListener main-canvas "mousedown" handle-canvas-mouse-down)

  (.addEventListener start-button "click" #(swap! state assoc :state :running))
  (.addEventListener pause-button "click" #(swap! state assoc :state :pause))
  (.addEventListener clean-button "click" #(swap! state assoc :world (gol/new-world)))
  (.addEventListener zoom-in-button "click" #(swap! state update :zoom * 2))
  (.addEventListener zoom-out-button "click" #(swap! state update :zoom / 2))

  (.addEventListener dump-button "click" #(prn (:world @state)))

  (.addEventListener examples-cases-select "change" #(swap! state assoc :world (gol/new-world (keyword (.-value (.-target %))))))

  (.requestAnimationFrame js/window main-loop))
