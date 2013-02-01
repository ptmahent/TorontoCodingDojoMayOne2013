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

(defn board-at-pos [board [xpos ypos]]
  (let [row (nth board ypos)]
    (nth row xpos)))

(def direction-factor {:up    [ 0 -1]
                       :down  [ 0  1]
                       :left  [-1  0]
                       :right [ 1  0]})

(defn pacman-move [board pos direction] 
  (let [board-dimensions (board-dim board)
        direction-factor (direction direction-factor)
        absolute-new-pos (plus pos direction-factor)
        potential-new-pos (map mod absolute-new-pos board-dimensions)
        board-at-potential-new-pos (board-at-pos board potential-new-pos)]
    (if (= 0 board-at-potential-new-pos) potential-new-pos pos)))


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
        partial-frame-val (partial-frame-gen)]
    (when (and (= 0 partial-frame-val) (state :pacman-pos)) 
      (reset! (state :pacman-pos) (pacman-move game-board @(state :pacman-pos) @(state :direction))))))

(def valid-keys {
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
    move (get valid-keys the-key-pressed :still)]
    (reset! (state :direction) move)))

(defn -main [& args]
	(defsketch pacman 
		:title "pacman"
		:setup setup
		:draw draw
		:key-pressed key-press
		:size [(:width params) (:height params)]))
