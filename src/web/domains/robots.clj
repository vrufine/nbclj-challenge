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
    (catch Exception e (throw e))))

(defn turn
  "Turn the given robot to the given direction"
  [robot direction]
  (if (g/is-valid-direction? direction)
    (reset! (get robot :facing) direction)
    (throw
      (Exception.
        "You tried to turn the robot to an invalid direction :( Try \"up\", \"down\", \"right\" or \"left\"."))))

(defn attack
  "Updates the given game reference with the new attacked grid and
  a new dinosaurs list (in case of destruction, one less dino to disturb!)"
  [game-ref robot]
  (try
    (let [grid-ref (get game-ref 'grid)
          direction @(get robot :facing)
          attacked-position (g/get-position-by-direction
                              direction {'x @(get robot :x)
                                         'y @(get robot :y)})
          attacked-x (get attacked-position 'x)
          attacked-y (get attacked-position 'y)
          new-dinos-list (remove
                           #(= {:Position {'x attacked-x 'y attacked-y}} %)
                           @(get game-ref 'dinos))]
      (dosync
        (ref-set (get game-ref 'grid) (g/update-cell @grid-ref attacked-x attacked-y "x"))
        (ref-set (get game-ref 'dinos) new-dinos-list)
        ))
    (catch Exception e (throw (Exception. (str "We could not process the attack. "
                                               "Check if the robot is facing a valid direction "
                                               "within the grid."))))))
