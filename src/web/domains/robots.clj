(ns web.domains.robots
  (:require [clojure.pprint :refer [pprint]]
            [web.domains.grid :as g]))

(defn create-robot
  [x y facing]
  (if (g/is-valid-direction? facing)
    {:x (atom x) :y (atom y) :facing (atom facing)}
    (throw
      (Exception.
        "You tried to create a robot facing an invalid direction :( Try \"up\", \"down\", \"right\" or \"left\"."))))

(defn move
  [grid robot]
  (try
    (let [old-pos {'x @(get robot :x) 'y @(get robot :y)}
          direction @(get robot :facing)
          {new-x 'x new-y 'y} (g/get-position-by-direction direction old-pos)]
      (let [new-grid (g/move-cell grid old-pos direction)
            new-robot (create-robot new-x new-y direction)]
        {'robot new-robot 'grid new-grid}))
    (catch Exception e (pprint (str "move-robot error: " (.getMessage e))))))

(defn turn
  "Turn the given robot to the given direction"
  [robot direction]
  (if (g/is-valid-direction? direction)
    (reset! (get robot :facing) direction)
    (throw
      (Exception.
        "You tried to turn the robot to an invalid direction :( Try \"up\", \"down\", \"right\" or \"left\"."))))

(defn attack
  "Returns a new grid that the cell attacked is reseted to the default value"
  [grid robot]
  (let [direction @(get robot :facing)
        attacked-position (g/get-position-by-direction direction {'x @(get robot :x)
                                                                  'y @(get robot :y)})]
    (try
      (g/update-cell grid (get attacked-position 'x) (get attacked-position 'y) "-")
      (catch Exception e (println (.getMessage e))))))
