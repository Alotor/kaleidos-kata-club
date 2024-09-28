(ns app.game-of-life-test
  (:require
   [app.game-of-life :as gol]
   [cljs.test :refer [deftest testing is are]]))

(deftest world-test
  []
  (testing "Create world"
    (let [world (gol/new-world)]
      (is (= (gol/alive? world 0 0) false))
      (is (= (gol/dead? world 0 0) true))
      (is (= (empty? (gol/walk-cells world)) true))))

  (testing "Make cells alive"
    (let [world (-> (gol/new-world)
                    (gol/make-alive 1 1))]
      (is (= (gol/alive? world 1 1) true))
      (is (= (gol/dead? world 1 1) false))
      (is (= (empty? (gol/walk-cells world)) false))))

  (testing "Make cells dead"
    (let [world (-> (gol/new-world)
                    (gol/make-alive 1 1)
                    (gol/make-dead 1 1))]
      (is (= (gol/alive? world 1 1) false))
      (is (= (gol/dead? world 1 1) true))
      (is (= (empty? (gol/walk-cells world)) true))))


  (testing "Processing cells"
    (testing "- Any live cell with fewer than two live neighbours dies, as if by underpopulation."
      (testing "- No neihbours"
        (let [world (-> (gol/new-world)
                        (gol/make-alive 0 0)
                        (gol/process-world))]
          (is (= (gol/dead? world 0 0) true))))
      
      (testing "- One neighbour"
        (let [world (-> (gol/new-world)
                        (gol/make-alive 0 0)
                        (gol/make-alive 0 1)
                        (gol/process-world))]
          (is (= (gol/dead? world 0 0) true)))))

    (testing "Any live cell with two or three live neighbours lives on to the next generation."
      (testing "- Two neighbours"
        (let [world (-> (gol/new-world)
                        (gol/make-alive 0 0)
                        (gol/make-alive 0 1)
                        (gol/make-alive 1 0)
                        (gol/process-world))]
          (is (= (gol/alive? world 0 0) true))))

      (testing "- Three neighbours"
        (let [world (-> (gol/new-world)
                        (gol/make-alive 0 0)
                        (gol/make-alive 0 1)
                        (gol/make-alive 1 0)
                        (gol/make-alive -1 0)
                        (gol/process-world))]
          (is (= (gol/alive? world 0 0) true)))))

    (testing "Any live cell with more than three live neighbours dies, as if by overpopulation."
      (testing "- Four neighbours"
        (let [world (-> (gol/new-world)
                        (gol/make-alive 0 0)
                        (gol/make-alive 0 1)
                        (gol/make-alive 1 0)
                        (gol/make-alive -1 0)
                        (gol/make-alive 0 -1)
                        (gol/process-world))]
          (is (= (gol/alive? world 0 0) false)))))

    (testing "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction."
      (let [world (-> (gol/new-world)
                      (gol/make-alive 0 1)
                      (gol/make-alive 1 0)
                      (gol/make-alive -1 0)
                      (gol/process-world))]
        (is (= (gol/alive? world 0 0) true))))))
