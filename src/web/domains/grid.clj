(ns web.domains.grid
  (:require [clojure.pprint :refer [pprint]]))

(defn create-grid
  "Returns a new grid given the width and height."
  [size]
  (to-array-2d (repeat size (repeat size "-"))))

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

(defn switch-cells
  "Use given grid reference to switch the cells"
  [grid from to]
  (let [{from-x 'x
         from-y 'y} from
        {to-x 'x
         to-y 'y} to
        from-value (aget grid from-x from-y)
        to-value (aget grid to-x to-y)]
    (if (and (is-valid-position? grid from-x from-y) (is-valid-position? grid to-x to-y))
      (do
        (aset grid to-x to-y from-value)
        (aset grid from-x from-y to-value)
        grid)
      (throw (Exception. "You are trying to move from (or to) an invalid grid position :(")))))

(defn get-cell-value
  "Returns the given x-y cell value"
  [grid x y]
  (if (is-valid-position? grid x y)
    (aget grid x y)
    (throw (Exception. (str "Get cell value: invalid position.")))))

(defn cell-is-empty?
  "Checks if the given cell is empty and something can be placed"
  [grid x y]
  (or (= "x" (get-cell-value grid x y)) (= "-" (get-cell-value grid x y))))

(defn stringify-robot [robot]
  (str "Robot facing " (cond
                         (= "up" @(get robot 'facing)) "⬆"
                         (= "down" @(get robot 'facing)) "⬇"
                         (= "left" @(get robot 'facing)) "⬅"
                         (= "right" @(get robot 'facing)) "➡"
                         )))

(defn stringify-grid
  "Return the stringified grid"
  [grid]
  (let [grid-str (atom "<table cellspacing='0' cellpadding='5' border='1'><tbody>")
        grid-size-x (alength grid)
        grid-size-y (alength (aget grid 0))]
    (do
      (doseq [y (range grid-size-y)]
        (reset! grid-str (str @grid-str "<tr>"))
        (doseq [x (range grid-size-x)]
          (reset! grid-str (str @grid-str "<td> (X" x ", Y" y "): <br/>" (if (not (nil? (get (get-cell-value grid x y) 'facing)))
                                                                           (stringify-robot (get-cell-value grid x y))
                                                                           (get-cell-value grid x y)) "</td>")))

        (reset! grid-str (str @grid-str "</tr>")))
      (reset! grid-str (str @grid-str "</tbody></table>")))))

(defn merge-grids
  "Merge grids X and Y axis"
  [old-grid new-grid]
  (let [size (alength old-grid)
        returning-grid (create-grid size)]
    (do
      (doseq [x (range size)]
        (doseq [y (range size)]
          (let [new-value (aget new-grid x y)
                old-value (aget old-grid x y)]
            (cond
              (and
                (= "-" old-value)
                (= "-" new-value)) (aset returning-grid x y old-value)

              (and
                (not= old-value new-value)
                (not= "-" new-value)) (aset returning-grid x y new-value)

              (and
                (not= old-value new-value)
                (not= "-" old-value)) (aset returning-grid x y old-value)

              :else (aset returning-grid x y new-value)))))
      returning-grid)))

(defn update-cell
  "Returns a new grid with the cell updated value."
  [old-grid x y value]
  (if (is-valid-position? old-grid x y)
    (let [new-grid (create-grid (alength old-grid))]
      (do
        (aset new-grid x y value)
        ;(when (or (= x 4) (= x 0)) (do
        ;                             (prn "old grid    => " (stringify-grid old-grid))
        ;                             (prn "new grid    => " (stringify-grid new-grid))
        ;                             (prn "merged grid =>" (stringify-grid (merge-grids old-grid new-grid)))
        ;                             (merge-grids old-grid new-grid)))
        (let [merged-grid (merge-grids old-grid new-grid)]
          (do
            ;(prn (stringify-grid merged-grid))
            merged-grid))))
    (throw (Exception. "Invalid position."))))


(defn move-cell-value
  "Move the cell value from one to another cell"
  [old-pos new-pos original-grid]
  (let [{old-x 'x old-y 'y} old-pos
        {new-x 'x new-y 'y} new-pos]
    (if (and
          (is-valid-position? original-grid old-x old-y)
          (is-valid-position? original-grid new-x new-y)
          (cell-is-empty? original-grid new-x new-y))
      (switch-cells original-grid {'x old-x 'y old-y} {'x new-x 'y new-y})
      (throw (Exception. "Error while moving the cell value.")))))

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
    (throw (Exception. "This position isn't available. Try another one please! :)"))))