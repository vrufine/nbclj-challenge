(ns web.domains.dinosaurs-test
  (:require [clojure.test :refer :all]
            [web.domains.grid :as g]
            [web.domains.dinosaurs :as d]))

(deftest dino-test
  (testing
    "Test if the dinosaur is created correctly
    into the grid, without changing the original grid."
    (let [grid (g/create-grid 10)
          grid-with-dinosaur (d/create-dinosaur grid {'x 3 'y 4})]
      (do
        (is (= (g/get-cell-value grid 3 4) "-"))
        (is (= (g/get-cell-value grid-with-dinosaur 3 4) "D"))))))