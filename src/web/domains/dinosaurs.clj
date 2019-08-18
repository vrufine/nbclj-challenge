(ns web.domains.dinosaurs
  (:require [web.domains.grid :as g]))

(defn create-dinosaur
  "Returns a new grid containing a dinosaur created
  within the cell positioned on the given X and Y."
  [old-grid position-to-place]
  (let [{x 'x, y 'y} position-to-place
        new-grid (g/create-grid (alength old-grid))]
    (g/place-new-element new-grid x y "D")))
