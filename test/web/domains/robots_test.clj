(ns web.domains.robots-test
  (:require [clojure.test :refer :all]
            [web.domains.robots :as r]
            [web.domains.grid :as g]))

(deftest robots-manipulation
  (testing
    "Create a new robot"
    (let [robot (r/create-robot 3 4 "right")]
      (try
        (is (= 3 @(get robot :x)))
        (is (= 4 @(get robot :y)))
        (is (= "right" @(get robot :facing)))
        (catch Exception e (is (nil? e))))))

  (testing
    "Place a new robot into a grid"
    (let [robot (r/create-robot 3 4 "right")
          grid (g/create-grid 10)]
      (try
        (let
          [grid-with-robot (g/place-new-element grid 3 4 robot)
           robot-cell (g/get-cell-value grid-with-robot 3 4)]
          (do
            (is (= robot robot-cell))))
        (catch Exception e (is (nil? e))))))

  (testing
    "Move the robot within the grid"
    (let [robot (r/create-robot 3 4 "right")
          grid (g/create-grid 10)]
      (try
        (let
          [grid-with-robot (g/place-new-element grid 3 4 robot)
           robot-cell (g/get-cell-value grid-with-robot 3 4)]
          (do
            (is (= robot robot-cell))
            (is (= 3 @(get robot :x)))
            (is (= 4 @(get robot :y)))

            (let [{grid-after-robot-move 'grid
                   robot-after-move 'robot} (r/move grid-with-robot robot)]
              (do
                (is (= 4 @(get robot-after-move :x)))
                (is (= 4 @(get robot-after-move :y)))
                (is (= "-" (g/get-cell-value grid-after-robot-move 3 4)))
                (is (not= "-" (g/get-cell-value grid-after-robot-move 4 4)))))))
        (catch Exception e (is (nil? e))))))

  (testing
    "Make a robot attack the direction he is facing"
    (try
      (let [robot (r/create-robot 5 4 "left")
            grid (g/create-grid 10)]
        (let [grid-with-robot (g/update-cell grid 4 4 "R")]
          (do
            (is (= "R" (g/get-cell-value grid-with-robot 4 4)))
            (let [grid-with-attacked-cell (r/attack grid-with-robot robot)]
              (do
                (is (= "-" (g/get-cell-value grid-with-attacked-cell 4 4))))))
          )
        )
      (catch Exception e (println (.getMessage e)))))

  (testing
    "Turn the facing direction of a robot"
    (try
      (let [robot (r/create-robot 5 4 "left")
            grid (g/create-grid 10)]
        (do
          (g/place-new-element grid 5 4 robot)
          (is (= "left" @(get robot :facing)))
          (r/turn robot "right")
          (is (= "right" @(get robot :facing)))))
      (catch Exception e (println (.getMessage e))))))
