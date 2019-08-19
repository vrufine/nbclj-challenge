(ns web.domains.game-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [web.domains.game :as game]
            [web.domains.robots :as robots]
            [web.domains.grid :as grid]))

(deftest game-creation
  (testing "The game is created correctly with empty cells (all values equals to '-')"
    (try
      (let [game (game/new-game)
            game-robots-ref (get game 'robots)
            game-dinos-ref (get game 'dinos)
            game-grid-ref (get game 'grid)
            game-grid-size (alength @game-grid-ref)]
        (do
          (is (= 0 (count @game-robots-ref)))
          (is (= 0 (count @game-dinos-ref)))

          (doseq [x (range game-grid-size)]
            (doseq [y (range game-grid-size)]
              (is (= "-" (aget @game-grid-ref x y)))))))
      (catch Exception e (is (nil? e)))))
  )

(deftest game-positioning
  (testing "The game grid accepts a new robot at position (x 15 y 15)"
    (try
      (let [game-ref (game/new-game)
            game-robots-ref (get game-ref 'robots)
            game-dinos-ref (get game-ref 'dinos)
            game-grid-ref (get game-ref 'grid)
            game-grid-size (alength @game-grid-ref)
            new-robot (robots/create-robot 15 15 "up")]
        (dosync
          (game/place-robot game-ref new-robot)
          (is (= new-robot (grid/get-cell-value @game-grid-ref 15 15)))
          (is (= 1 (count @game-robots-ref)))
          (is (= 0 (count @game-dinos-ref)))
          (is (= 15 @(get (nth @game-robots-ref 0) :x)))
          (is (= 15 @(get (nth @game-robots-ref 0) :y)))
          (is (= "up" @(get (nth @game-robots-ref 0) :facing)))))
      (catch Exception e (do
                           (prn "=The game grid accepts a new robot at position (x 15 y 15)= thrown error: "
                                (.getMessage e))
                           (is (nil? e))))))

  (testing "The game grid accepts a new dinosaur at (x 16 y 15)"
    (try
      (let [game-ref (game/new-game)
            game-robots-ref (get game-ref 'robots)
            game-dinos-ref (get game-ref 'dinos)
            game-grid-ref (get game-ref 'grid)]
        (dosync
          (game/place-dino game-ref 16 15)

          (is (= "D" (aget @game-grid-ref 16 15)))
          (is (= 0 (count @game-robots-ref)))
          (is (= 1 (count @game-dinos-ref)))

          ))
      (catch Exception e (do
                           (println "game-positioning x threw an error: " (.getMessage e))
                           (is (nil? e))))))

  (testing "Should throw error when trying to add a new robot at the same position (x 15 y 15)"
    (try
      (let [game-ref (game/new-game)
            robot-1 (robots/create-robot 15 15 "up")
            robot-2 (robots/create-robot 15 15 "right")
            robot-3 (robots/create-robot 20 20 "left")]
        (dosync
          (game/place-robot game-ref robot-1)
          (game/place-robot game-ref robot-2)
          (game/place-robot game-ref robot-3)
          (is (= "" "This assertion should not be executed."))))
      (catch Exception e
        (is (not= true (nil? e)))
        (is (= "This position isn't available. Try another one please! :)" (.getMessage e)))))))

(deftest game-interactions
  (testing "The grid cell where the dinosaur is, should be empty after a robot attack"
    (try
      (let [game-ref (game/new-game)
            grid-ref (get game-ref 'grid)
            robots-ref (get game-ref 'robots)
            dinos-ref (get game-ref 'dinos)
            assassin-robot-killer (robots/create-robot 10 16 "down")]
        (dosync
          (is (= 0 (count @robots-ref)))
          (is (= 0 (count @dinos-ref)))
          (is (true? (grid/cell-is-empty? @grid-ref 10 15)))
          (is (true? (grid/cell-is-empty? @grid-ref 10 16)))

          (game/place-dino game-ref 10 15)
          (is (= 1 (count @dinos-ref)))
          (is (not (true? (grid/cell-is-empty? @grid-ref 10 15))))

          (game/place-robot game-ref assassin-robot-killer)
          (is (= 1 (count @robots-ref)))
          (is (not (true? (grid/cell-is-empty? @grid-ref 10 16))))

          (dosync
            (is (= "D" (grid/get-cell-value @grid-ref 10 15)))
            (robots/attack game-ref assassin-robot-killer)
            (is (= 0 (count @dinos-ref)))
            (is (true? (grid/cell-is-empty? @grid-ref 10 15))))))
      (catch Exception e (do
                           (println "dino death test error: " (.getMessage e))
                           (is (nil? e))))))

  (testing "Should throw error if the robot attacks an invalid grid cell"
    (try
      (let [game-ref (game/new-game)
            robot-0-0 (robots/create-robot 0 0 "down")
            robot-0-49 (robots/create-robot 0 49 "left")
            robot-49-0 (robots/create-robot 49 0 "right")
            robot-49-49 (robots/create-robot 49 49 "up")]
        (do
          (try
            (game/place-robot game-ref robot-0-0)
            (robots/attack game-ref robot-0-0)
            (catch Exception e
              (let [error-message (.getMessage e)]
                (do
                  (is (not (nil? e)))
                  (is
                    (= (str "We could not process the attack. "
                            "Check if the robot is facing a valid direction within the grid.")
                       error-message))))))

          (try
            (game/place-robot game-ref robot-0-49)
            (robots/attack game-ref robot-0-49)
            (catch Exception e
              (let [error-message (.getMessage e)]
                (do
                  (is (not (nil? e)))
                  (is
                    (= (str "We could not process the attack. "
                            "Check if the robot is facing a valid direction within the grid.")
                       error-message))))))

          (try
            (game/place-robot game-ref robot-49-0)
            (robots/attack game-ref robot-49-0)
            (catch Exception e
              (let [error-message (.getMessage e)]
                (do
                  (is (not (nil? e)))
                  (is
                    (= (str "We could not process the attack. "
                            "Check if the robot is facing a valid direction within the grid.")
                       error-message))))))

          (try
            (game/place-robot game-ref robot-49-49)
            (robots/attack game-ref robot-49-49)
            (catch Exception e
              (let [error-message (.getMessage e)]
                (do
                  (is (not (nil? e)))
                  (is
                    (= (str "We could not process the attack. "
                            "Check if the robot is facing a valid direction within the grid.")
                       error-message))))))
          ))
      (catch Exception e
        (let [error-message (.getMessage e)]
          (println "Unexpected error ==> " error-message)))))

  (testing "The game grid should update cells after the a robot move"
    (let [game-ref (game/new-game)
          robot (robots/create-robot 20 20 "right")]
      (do
        (game/place-robot game-ref robot)
        (let [grid-copy (aclone @(get game-ref 'grid))]
          (is (= robot (grid/get-cell-value grid-copy 20 20)))
          (is (= "-" (grid/get-cell-value grid-copy 21 20)))
          (robots/move @(get game-ref 'grid) robot)
          (is (= "-" (grid/get-cell-value grid-copy 20 20)))
          (is (= robot (grid/get-cell-value grid-copy 21 20))))))))
