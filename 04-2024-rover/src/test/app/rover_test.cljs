(ns app.rover-test
  (:require
   [app.rover :as rover]
   [cljs.test :refer [deftest testing is are]]))

;;You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing.

(deftest initialize-rover
  []
  (testing "Rover initialization and basic access"
    (let [r (rover/init 10 10 rover/EAST)]
      (is (= (rover/x r) 10))
      (is (= (rover/y r) 10))
      (is (= (rover/dir r) rover/EAST)))))

;; The rover receives a character array of commands.
(deftest commands
  []
  (testing "Rover single command from 10,10"
    (are [input expected]
        (let [world (rover/world 20 20 [])
              [direction command] input
              r (-> (rover/init 10 10 direction)
                    (rover/commands world [command]))]
          (= expected [(rover/dir r) (rover/x r) (rover/y r)]))

      ;; Implement commands that move the rover forward/backward (f,b).
      [rover/NORTH "f"] [rover/NORTH 10 11]
      [rover/EAST "f"]  [rover/EAST 11 10]
      [rover/SOUTH "f"] [rover/SOUTH 10 9]
      [rover/WEST "f"]  [rover/WEST 9 10]

      [rover/NORTH "b"] [rover/NORTH 10 9]
      [rover/EAST "b"]  [rover/EAST 9 10]
      [rover/SOUTH "b"] [rover/SOUTH 10 11]
      [rover/WEST "b"]  [rover/WEST 11 10]
      
      ;; Implement commands that turn the rover left/right (l,r).
      [rover/NORTH "l"] [rover/WEST 10 10]
      [rover/EAST "l"]  [rover/NORTH 10 10]
      [rover/SOUTH "l"] [rover/EAST 10 10]
      [rover/WEST "l"]  [rover/SOUTH 10 10]
      
      [rover/NORTH "r"] [rover/EAST 10 10]
      [rover/EAST "r"]  [rover/SOUTH 10 10]
      [rover/SOUTH "r"] [rover/WEST 10 10]
      [rover/WEST "r"]  [rover/NORTH 10 10]))

  (testing "Rover command chain"
    (are [input expected]
        (let [world (rover/world 20 20 [])
              [direction commands] input
              r (-> (rover/init 10 10 direction)
                    (rover/commands world commands))]
          (= expected [(rover/dir r) (rover/x r) (rover/y r)]))

      ;; Implement commands that move the rover forward/backward (f,b).
      [rover/NORTH ["f" "f" "f" "b" "b" "b"]] [rover/NORTH 10 10]
      [rover/EAST ["f" "f" "f" "b" "b" "b"]] [rover/EAST 10 10]
      [rover/WEST ["f" "f" "f" "b" "b" "b"]] [rover/WEST 10 10]
      [rover/SOUTH ["f" "f" "f" "b" "b" "b"]] [rover/SOUTH 10 10]

      [rover/NORTH ["f" "f" "l" "l" "f" "f"]] [rover/SOUTH 10 10]
      [rover/EAST ["f" "f" "l" "l" "f" "f"]] [rover/WEST 10 10]
      [rover/WEST ["f" "f" "l" "l" "f" "f"]] [rover/EAST 10 10]
      [rover/SOUTH ["f" "f" "l" "l" "f" "f"]] [rover/NORTH 10 10]

      [rover/NORTH ["f" "l" "f" "l" "f" "l" "f"]] [rover/EAST 10 10]
      [rover/EAST ["f" "l" "f" "l" "f" "l" "f"]] [rover/SOUTH 10 10]
      [rover/SOUTH ["f" "l" "f" "l" "f" "l" "f"]] [rover/WEST 10 10]
      [rover/NORTH ["f" "l" "f" "l" "f" "l" "f"]] [rover/EAST 10 10]))

  (testing "Rover wrapping world"
    (are [input expected]
        (let [world (rover/world 10 10 [])
              [start-x start-y direction command] input
              r (-> (rover/init start-x start-y direction)
                    (rover/commands world [command]))]
          (= expected [(rover/dir r) (rover/x r) (rover/y r)]))

      ;; Implement commands that move the rover forward/backward (f,b).
      [0  9 rover/NORTH "f"] [rover/NORTH 0 0]
      [9  0 rover/EAST  "f"] [rover/EAST  0 0]
      [0  0 rover/WEST  "f"] [rover/WEST  9 0]
      [0  0 rover/SOUTH "f"] [rover/SOUTH 0 9]

      [0  9 rover/SOUTH "b"] [rover/SOUTH 0 0]
      [9  0 rover/WEST  "b"] [rover/WEST  0 0]
      [0  0 rover/EAST  "b"] [rover/EAST  9 0]
      [0  0 rover/NORTH "b"] [rover/NORTH 0 9]))

  (testing "Obstacles"
    (are [input expected]
        (let [world (rover/world 20 20 [[10 10]])
              [start-x start-y direction command] input
              r (try
                  (-> (rover/init start-x start-y direction)
                      (rover/commands world [command]))
                  (catch :default e "ERROR"))]
          (= expected
             (if (= r "ERROR")
               "ERROR"
               [(rover/dir r) (rover/x r) (rover/y r)])))

      ;; Implement commands that move the rover forward/backward (f,b).
      [9   9  rover/NORTH "f"] [rover/NORTH 9 10]
      [10  9  rover/NORTH "f"] "ERROR"
      [10  11 rover/NORTH "b"] "ERROR"
      
      [9   11  rover/SOUTH "f"] [rover/SOUTH 9 10]
      [10  11  rover/SOUTH "f"] "ERROR"
      [10  9   rover/SOUTH "b"] "ERROR"

      [9   9  rover/EAST "f"] [rover/EAST 10 9]
      [9  10  rover/EAST "f"] "ERROR"
      [11  10 rover/EAST "b"] "ERROR"
      
      [11   9  rover/WEST "f"] [rover/WEST 10 9]
      [11  10  rover/WEST "f"] "ERROR"
      [9   10 rover/WEST "b"] "ERROR")))


