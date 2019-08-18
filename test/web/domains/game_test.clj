(ns web.domains.game-test
  (:require [clojure.test :refer :all]
            [web.domains.game :as game]))

(deftest game-creation
  (testing "The game is created correctly with empty cells (all values equals to '-')")

  (testing "Should throw error if the size parameter is not a number"))

(deftest game-positioning
  (testing "The game grid accepts a new robot at position (x 15 y 15)")

  (testing "The game grid accepts a new dinosaur at (x 16 y 15)")

  (testing "Should throw error when trying to add a new robot at the same position (x 15 y 15)"))

(deftest game-interactions
  (testing "The grid cell where the dinosaur is, should be empty (value '-') after a robot attack")

  (testing "Should throw error if the robot attacks an invalid grid cell")

  (testing "The game grid should update cells after the a robot move"))
