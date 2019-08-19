(ns web.domains.game
  (:require [web.domains.grid :as grid]))

(defonce ^:private current-game (ref nil))

(defn new-game []
  (try
    (let [grid (grid/create-grid 50)]
      (dosync
        (ref-set current-game {'grid   (ref grid)
                               'robots (ref (sequence []))
                               'dinos  (ref (sequence []))})
        current-game))
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

(defn get-current-game []
  (if (not (nil? @current-game))
    (do
      (println "Html type: " (type (grid/stringify-grid @(get @current-game 'grid))))
      (println "Robots sequence: " (if (> 0 (count @(get @current-game 'robots)))
                                     (map
                                       (fn [r] (do @r))
                                       @(get @current-game 'robots))
                                     (sequence [])))
      (println "Dinos sequence: " (if (> 0 (count @(get @current-game 'dinos)))
                                    (map
                                      (fn [r] (do @r))
                                      @(get @current-game 'dinos))
                                    (sequence [])))

      {'html (grid/stringify-grid @(get @current-game 'grid))
       'ref  current-game
       'data {'grid   @(get @current-game 'grid)
              'robots (if (> 0 (count @(get @current-game 'robots)))
                        (map
                          (fn [r] (do @r))
                          @(get @current-game 'robots))
                        nil)
              'dinos  (if (> 0 (count @(get @current-game 'dinos)))
                        (map
                          (fn [r] (do @r))
                          @(get @current-game 'dinos))
                        nil)
              }})

    {'html "No game to show"
     'ref  nil
     'data nil}))