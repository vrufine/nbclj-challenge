(ns web.domains.game
  (:require [web.domains.grid :as grid]))

(defn new-game []
  (try
    (let [grid (grid/create-grid 50)]
      (do
        {'grid   (ref grid)
         'robots (ref (sequence []))
         'dinos  (ref (sequence []))}))
    (catch Exception e (throw (Exception. "Error while we tried to create a new game :(")))))

(defn get-game-robots [game-ref]
  (try
    @(get game-ref 'robots)
    (catch Exception e
      (println "Get Game Robots Error: " (.getMessage e)))))

(defn get-game-dinos [game-ref]
  (try
    @(get game-ref 'dinos)
    (catch Exception e
      (println "Get Game Dinos Error: " (.getMessage e)))))

(defn place-robot
  "Changes the grid-ref reference to place a robot on the grid "
  [game-ref new-robot]
  (try
    (let [grid-ref (get game-ref 'grid)
          robots-ref (get game-ref 'robots)
          x @(get new-robot :x)
          y @(get new-robot :y)]
      (dosync
        (ref-set robots-ref (concat @robots-ref (seq [new-robot])))
        (ref-set grid-ref (grid/place-new-element @grid-ref x y new-robot))
        game-ref))
    (catch Exception e (throw e))))

(defn place-dino
  "Updates the grid (inside game-ref) to insert a new dinosaur on grid"
  [game-ref x y]
  (try
    (let [grid-ref (get game-ref 'grid)
          dinos-ref (get game-ref 'dinos)]
      (dosync
        (ref-set grid-ref (grid/place-new-element @grid-ref x y "D"))
        (ref-set dinos-ref (concat @dinos-ref (seq [{:Position {'x x 'y y}}])))
        game-ref))
    (catch Exception e (throw e))))
