(ns tompac.core
  (:use [quil.core] 
        [quil.helpers.seqs]
        [incanter.core :only [matrix dim plus matrix-map]])
  (:require spyscope.core)
  (:import java.awt.event.KeyEvent)
  (:gen-class))

(defn board-dim [board]
	; reverse to be consistent with pos
  ; i.e. pos is [x y] and dim is [width height]
  (reverse (dim (matrix board))))

(defn board-width [board]
  (-> board board-dim first))

(defn board-height [board]
  (-> board board-dim second))

(defn dimensions [board]
  [(board-width board) (board-height board)])

(def delta {:up    -1
            :right  1
            :down   1
            :left  -1})

(def axis {:up   :vertical
           :down :vertical
           :left  :horizontal
           :right :horizontal})

(defn update-first [s update]
  (vector (update (first s)) (second s)))

(defn update-second [s update]
  (vector (first s) (update (second s))))

(defn next-pos [pos direction]
  (case (direction axis) 
    :horizontal (update-first pos (partial + (direction delta)))
    :vertical  (update-second pos (partial + (direction delta)))
    pos))

(defn wrap [dimensions pos]
  (-> pos 
      (update-first  (fn [x] (mod x (first dimensions))))
      (update-second (fn [x] (mod x (second dimensions)))))) 

(defn move [board pos direction] 
  (wrap (dimensions board) (next-pos pos direction)))

(defn board-at-pos [board pos]
  (let [xpos (first pos)
        ypos (second pos)
        row  (nth board ypos)]
    (nth row xpos)))

(defn open? [board pos direction] 
  (= 0 (board-at-pos board (move board pos direction))))


;; Model up from here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics here on down


(def params {:width  320
             :height 400})

(def game-board [[1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                 [1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1]
                 [1 0 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 0 1]
                 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
                 [1 0 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 0 1]
                 [1 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 1]
                 [1 1 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 1]
                 [1 1 1 1 0 1 0 0 0 0 0 0 0 1 0 1 1 1 1]
                 [1 1 1 1 0 1 0 1 1 2 1 1 0 1 0 1 1 1 1]
                 [0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0]
                 [1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1]
                 [1 1 1 1 0 1 0 0 0 0 0 0 0 1 0 1 1 1 1]
                 [1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1]
                 [1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1]
                 [1 0 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 0 1]
                 [1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1]
                 [1 1 0 1 0 1 0 1 1 1 1 1 0 1 0 1 0 1 1]
                 [1 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 1]
                 [1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1]
                 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
                 [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]])

(def game-board-width (board-width game-board))
(def game-board-height (board-height game-board))

(def cell-width  (/ (:width  params) game-board-width))
(def cell-height (/ (:height params) game-board-height))

(def half-cell-width  (/ cell-width 2))
(def half-cell-height (/ cell-height 2))

(defn setup []
	(smooth)
	(frame-rate 60)
	(set-state!
      :pacman-pos (atom [1 1]) 
			:direction (atom :left)
			:partial-frame (seq->stream (cycle-between 0 5))))

(defn cell-top-x [x] (* x cell-width))
(defn cell-top-y [y] (* y cell-height))
(defn cell-center-x [x] (+ (cell-top-x x) half-cell-width))
(defn cell-center-y [y] (+ (cell-top-y y) half-cell-height))

(defn draw-rect [x y width height]
  (rect (cell-top-x x) (cell-top-y y) width height))

(defn draw-wall-brick [x y]
  (fill 0 0 255)
  (draw-rect x y cell-width cell-height))

(defn draw-ghosts-door [x y]
  (fill 255 255 255)
  (draw-rect x y cell-width (/ cell-height 4)))

(defn draw []
	(background 0)
  
  ; The Grid
  (doseq [[y row] (map-indexed vector game-board)]
    (doseq [[x value] (map-indexed vector row)]
      (case 
        value
        1 (draw-wall-brick x y)
        2 (draw-ghosts-door x y)
        nil)))

  ; Pac Man
  (fill 255 255 0)
  (ellipse (cell-center-x (first @(state :pacman-pos))) 
           (cell-center-y (second @(state :pacman-pos)))
           cell-width
           cell-height)  
  
	(let [partial-frame-gen (state :partial-frame)
        partial-frame-val (partial-frame-gen)
        potential-next-pos (move game-board @(state :pacman-pos) @(state :direction))]
    (when (and (= 0 partial-frame-val) (open? game-board @(state :pacman-pos) @(state :direction)))
      (reset! (state :pacman-pos) potential-next-pos))  
		(text (str partial-frame-val @(state :direction)) 20 60)))

(def key-directions {
  KeyEvent/VK_UP :up
  KeyEvent/VK_DOWN :down
  KeyEvent/VK_LEFT :left
  KeyEvent/VK_RIGHT :right
  \w :up
  \s :down
  \a :left
  \d :right})

(defn key-press []
  (let [raw-key (raw-key)
    the-key-code (key-code)
    the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)
    key-direction (get key-directions the-key-pressed nil)]
    (when (and key-direction (open? game-board @(state :pacman-pos) key-direction))
      (reset! (state :direction) key-direction))))

(defn -main [& args]
	(defsketch pacman 
		:title "pacman"
		:setup setup
		:draw draw
		:key-pressed key-press
		:size [(:width params) (:height params)]))
