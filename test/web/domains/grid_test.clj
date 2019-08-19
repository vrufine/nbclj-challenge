(ns web.domains.grid-test
  (:require [clojure.test :refer :all]
            [web.domains.grid :refer :all]))

(deftest grid-manipulation-test
  (testing
    "Grid is created with the correct size (10x10)"
    (let [grid-tested (create-grid 10)
          cell-1 (try (get-cell-value grid-tested 0 0)
                      (catch Exception e (.getMessage e)))
          cell-2 (try (get-cell-value grid-tested 9 9)
                      (catch Exception e (.getMessage e)))]
      (is (= 10 (alength grid-tested)))
      (is (= 10 (alength (aget grid-tested 0))))
      (is (= "-" cell-1))
      (is (= "-" cell-2))))

  (testing
    "The cell (x 3, y 4) has the original \"-\" value"
    (let [grid-tested (create-grid 10)
          cell-value (try
                       (get-cell-value grid-tested 3 4)
                       (catch Exception e (.getMessage e)))]
      (do
        (is (not=
              "Invalid cell position. Try again with another X and Y values."
              cell-value))
        (is (= "-" cell-value)))))

  (testing
    "Grid cloning"
    (let [grid (create-grid 5)]
      (let [updated-grid (update-cell grid 3 4 "Yes")]
        (do
          (is (= "-" (get-cell-value grid 3 4)))
          (is (= "Yes" (get-cell-value updated-grid 3 4)))))))

  (testing
    "The cell (x 3, y 4) receives the new value \"RR\""
    (let [grid (create-grid 10)]
      (try
        (let [updated-grid (place-new-element grid 3 4 "RR")]
          (do
            (is (not= "RR" (get-cell-value grid 3 4)))
            (def x (get-cell-value updated-grid 3 4))
            (is (= "RR" (get-cell-value updated-grid 3 4)))))
        (catch Exception e (is (nil? e))))))

  (testing
    "The grid is properly stringified"
    (let [grid-tested (create-grid 10)
          grid-string (stringify-grid grid-tested)]
      (is (= String (type grid-string)))))

  (testing
    "The content of a cell is moved to another cell"
    (let [grid-tested (create-grid 10)
          old-cell-pos {'x 3 'y 4}
          new-cell-pos {'x 2 'y 9}
          fake-robot {:Direction "right"}]
      (try
        (let [updated-grid (update-cell grid-tested 3 4 fake-robot)]
          (do
            (is (= {:Direction "right"} (try (get-cell-value updated-grid 3 4)
                                             (catch Exception e (.getMessage e)))))
            (is (= "-" (try (get-cell-value updated-grid 2 9)
                            (catch Exception e (.getMessage e)))))
            (let [up2 (move-cell-value old-cell-pos new-cell-pos updated-grid)]
              (do
                (is (= "-" (try (get-cell-value up2 3 4)
                                (catch Exception e (.getMessage e)))))
                (is (= {:Direction "right"} (try (get-cell-value up2 2 9)
                                                 (catch Exception e (.getMessage e)))))
                ))
            ))
        (catch Exception e (is (nil? e))))))

  (testing
    "The cell is moved upwards"
    (let [grid (create-grid 10)
          robot {:Direction "up" :Position {'x 2 'y 3}}]
      (try
        (let [updated-grid (update-cell grid 2 3 robot)]
          (do
            (is (= robot (get-cell-value updated-grid 2 3)))
            (is (= "-" (get-cell-value grid 2 4)))
            (let [moved-robot-grid (move-cell updated-grid (:Position robot) (:Direction robot))]
              (do
                (is (= "-" (get-cell-value moved-robot-grid 2 3)))
                (is (= robot (get-cell-value moved-robot-grid 2 4)))))))
        (catch Exception e (do
                             (clojure.pprint/pprint (.getMessage e))
                             (is (nil? e)))))))

  (testing
    "The cell is moved downwards"
    (let [grid (create-grid 10)
          robot {:Direction "down" :Position {'x 3 'y 4}}]
      (try
        (let [updated-grid-after-create-robot (update-cell grid 3 4 robot)]
          (do
            (is (= robot (get-cell-value updated-grid-after-create-robot 3 4)))
            (is (= "-" (get-cell-value updated-grid-after-create-robot 3 3)))
            (let [updated-grid-after-moving-robot (move-cell updated-grid-after-create-robot (:Position robot) (:Direction robot))]
              (do
                (is (= "-" (get-cell-value updated-grid-after-moving-robot 3 4)))
                (is (not= "-" (get-cell-value updated-grid-after-moving-robot 3 3)))))))

        (catch Exception e (is (nil? e))))))

  (testing
    "The cell is moved left"
    (let [grid (create-grid 10)
          robot {:Direction "left" :Position {'x 3 'y 4}}]
      (try
        (let [grid-with-robot (update-cell grid 3 4 robot)]
          (do
            (is (= robot (get-cell-value grid-with-robot 3 4)))
            (is (= "-" (get-cell-value grid-with-robot 2 4)))
            (let [grid-after-move (move-cell grid-with-robot (:Position robot) (:Direction robot))]
              (do
                (is (= "-" (get-cell-value grid-after-move 3 4)))
                (is (= robot (get-cell-value grid-after-move 2 4)))))))
        (catch Exception e (is (nil? e))))))

  (testing
    "The cell is moved right"
    (let [grid (create-grid 10)
          robot {:Direction "right" :Position {'x 3 'y 4}}]
      (try
        (let [grid-with-robot (update-cell grid 3 4 robot)]
          (do
            (is (= robot (get-cell-value grid-with-robot 3 4)))
            (is (= "-" (get-cell-value grid-with-robot 4 4)))
            (let [grid-after-move (move-cell grid-with-robot (:Position robot) (:Direction robot))]
              (do
                (is (= "-" (get-cell-value grid-after-move 3 4)))
                (is (= robot (get-cell-value grid-after-move 4 4)))))))
        (catch Exception e (is (nil? e))))))

  (testing
    "New grid is different from original grid"
    (let [grid (create-grid 10)
          grid-2 (create-grid 10)]
      (try
        (do
          (update-cell grid-2 3 3 "D")
          (is (= "-" (get-cell-value grid 3 3)))
          (let
            [new-grid (merge-grids grid grid-2)]
            (do
              (is (= "-" (get-cell-value grid 3 3)))
              (is (not= "D" (get-cell-value grid 3 3)))
              (is (not= "D" (get-cell-value new-grid 3 3)))
              (is (not (identical? grid new-grid))))))
        (catch Exception e (clojure.pprint/pprint (str (.getMessage e)))))))

  (testing
    "Set original grid value to new grid's value using ref"
    (let [original-grid (ref (create-grid 10))
          new-grid (place-new-element @original-grid 3 6 "New value at x3 y6 :)")]
      (do
        (is (not (identical? (aget @original-grid 3 6) (aget new-grid 3 6))))
        (dosync (ref-set original-grid new-grid))
        (is (identical? (aget @original-grid 3 6) (aget new-grid 3 6)))
        (-> (aget @original-grid 3 6)
            is
            (identical? (aget new-grid 3 6)))
        )))

  (testing "Place two elements simultaneously on the grid"
    (let [grid (atom (create-grid 5))]
      (do
        (is (= "-" (aget @grid 0 0)))
        (is (= "-" (aget @grid 4 4)))
        (reset! grid (place-new-element @grid 0 0 "A"))
        (is (= "A" (aget @grid 0 0)))
        (is (= "-" (aget @grid 4 4)))
        (reset! grid (place-new-element @grid 4 4 "B"))
        (is (= "A" (aget @grid 0 0)))
        (is (= "B" (aget @grid 4 4)))
        )))
  )
