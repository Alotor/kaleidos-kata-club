(ns app.bowling-test
  (:require
   [app.bowling :as b]
   [cljs.test :refer [deftest testing is are]]))

(deftest basic-points
  []

  (testing "Test bowling score"
    (are [input expected]
        (let [game (b/new-game)
              [game p1] (b/add-player game "Charmander")
              game (b/all-throws game p1 input)]
          (= expected (b/score game p1)))

      [ 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  0] 20
      [10  0  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  0] 30
      [10  0 10  0  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  0] 49
      [ 5  4  8  2 10  0 10  0  1  0  9  1  0 10 10  0  6  4  7  3 10] 149
      [ 8  2  5  4  9  0 10  0 10  0  5  5  5  3  6  3  9  1  9  1 10] 149
      [10  0  9  1  5  5  7  2 10  0 10  0 10  0  9  0  8  2  9  1 10] 187
      [10  0 10  0 10  0 10  0 10  0 10  0 10  0 10  0 10  0 10 10 10] 300
      )))






