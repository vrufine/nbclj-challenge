(ns web.domains.grid
  (:require [clojure.pprint :refer [pprint]]))

(defn create-grid
  "Returns a new grid given the width and height."
  [size]
  (to-array-2d (repeat size (repeat size "-"))))


(defn merge-grids
  "Merge grids X and Y axis"
  [old new]
  (let [size (alength old)
        new-grid (create-grid size)]
    (do
      (doseq [x (range size)]
        (doseq [y (range size)]
          (do
            (aset new-grid x y (aget new-grid x y)))))
      new)))

(defn is-valid-direction?
  [direction]
  (or
    (= "up" direction)
    (= "down" direction)
    (= "left" direction)
    (= "right" direction)))

(defn is-valid-position?
  "Checks if the given cell position is valid"
  [grid x y]
  (let [width-limit (alength grid)
        height-limit (alength (aget grid 0))]
    (and (< x width-limit) (< y height-limit) (>= x 0) (>= y 0))))

(defn get-position-by-direction
  "Returns {'x 'y} towards given the direction"
  [direction original-position]
  (let [{original-x 'x original-y 'y} original-position]
    (do
      (if (is-valid-direction? direction)
        (cond
          (= "up" direction) {'x original-x 'y (inc original-y)}
          (= "down" direction) {'x original-x 'y (dec original-y)}
          (= "right" direction) {'x (inc original-x) 'y original-y}
          (= "left" direction) {'x (dec original-x) 'y original-y}
          :else original-position)
        (throw (Exception. (str "Invalid position and/or direction")))))))

(defn get-cell-value
  "Returns the given x-y cell value"
  [grid x y]
  (if (is-valid-position? grid x y)
    (aget grid x y)
    (throw (Exception. (str "Get cell value: invalid position.")))))

(defn cell-is-empty?
  "Checks if the given cell is empty and something can be placed"
  [grid x y]
  (= "-" (get-cell-value grid x y)))

(defn update-cell
  "Returns a new grid with the cell updated value."
  [old-grid x y value]
  (if (is-valid-position? old-grid x y)
    (let [new-grid (create-grid (alength old-grid))]
      (do
        (aset new-grid x y value)
        (merge-grids old-grid new-grid)))
    (throw (Exception. "Update cell error: invalid position."))))

(defn stringify-grid
  "Return the stringified grid"
  [grid]
  (let [grid-str (atom "")
        grid-size-x (alength grid)
        grid-size-y (alength (aget grid 0))]
    (doseq [y (range grid-size-y)]
      (doseq [x (range grid-size-x)]
        (reset! grid-str (str @grid-str (get-cell-value grid x y) " ")))
      (reset! grid-str (str @grid-str "(y: " y ")\n")))
    @grid-str))

(defn move-cell-value
  "Move the cell value from one to another cell"
  [old-pos new-pos original-grid]
  (let [{old-x 'x old-y 'y} old-pos
        {new-x 'x new-y 'y} new-pos
        original-cell-value (get-cell-value original-grid old-x old-y)]
    (if (and
          (is-valid-position? original-grid old-x old-y)
          (is-valid-position? original-grid new-x new-y)
          (cell-is-empty? original-grid new-x new-y))
      (let
        [updated-old-cell-grid (update-cell original-grid old-x old-y "-")
         updated-new-cell-grid (update-cell updated-old-cell-grid new-x new-y original-cell-value)]
        (do
          updated-new-cell-grid))
      (throw (Exception. "Move cell value: error while updating cells.")))))

(defn move-cell
  "Move the given cell content onto the given direction"
  [grid cell-pos direction]
  (let [{curr-y 'y curr-x 'x} cell-pos]
    (cond
      (not (true? (is-valid-position? grid curr-x curr-y))) (throw (Exception. "The cell position does not exist."))

      (= direction "up") (let [new-pos (get-position-by-direction direction cell-pos)]
                           (do
                             (move-cell-value cell-pos new-pos grid)))

      (= direction "down") (let [new-pos (get-position-by-direction direction cell-pos)]
                             (do
                               (move-cell-value cell-pos new-pos grid)))

      (= direction "left") (let [new-pos (get-position-by-direction direction cell-pos)]
                             (do
                               (move-cell-value cell-pos new-pos grid)))

      (= direction "right") (let [new-pos (get-position-by-direction direction cell-pos)]
                              (do
                                (move-cell-value cell-pos new-pos grid)))

      :else (throw (Exception. (str "Move cell: invalid movement direction: \"" direction
                                    "\". Try \"up\", \"down\", \"left\" or \"right\"."))))))

(defn place-new-element
  "Place value into the x-y position of the given grid"
  [grid x y value]
  (if (true? (cell-is-empty? grid x y))
    (update-cell grid x y value)
    (throw (Exception. "Place new element: this cell isn't empty. Try another position :)"))))
