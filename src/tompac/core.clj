(ns tompac.core
  (:use [quil.core] 
        [quil.helpers.seqs]
        [incanter.core :only [matrix dim plus matrix-map]])
  (:require spyscope.core)
  (:import java.awt.event.KeyEvent)
  (:gen-class))

(defn board-width [board]
  (second (dim (matrix board))))

(defn board-height [board]
  (first (dim (matrix board))))

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

(defn wall? [board pos direction] 
  (= 1 (board-at-pos board (move board pos direction))))



;; Model up from here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics here on down


(def params {:width  320
             :height 200})

(def game-board [[0 1 1 0 0 0]
                 [0 1 1 0 0 0]
                 [0 0 0 0 1 0]
                 [0 0 0 0 1 0]
                 [0 0 0 0 0 0]])

(def game-board-width (board-width game-board))
(def game-board-height (board-height game-board))

(def cell-width  (/ (:width  params) game-board-width))
(def cell-height (/ (:height params) game-board-height))

(def half-cell-width  (/ cell-width 2))
(def half-cell-height (/ cell-height 2))

(defn setup []
	(smooth)
	(frame-rate 30)
	(set-state!
      :pacman-pos (atom [2 2]) 
			:direction (atom :left)
			:partial-frame (seq->stream (cycle-between 0 5))))

(defn identity2 [a b] [a b])

(defn cell-center-x [x] (+ (* x cell-width) half-cell-width))
(defn cell-center-y [y] (+ (* y cell-height) half-cell-height))

(defn draw []
	(background 0)
  
  ; The Grid
  (fill 0 0 255)
  (doseq [[y row] (map-indexed identity2 game-board)]
    (doseq [[x value] (map-indexed identity2 row)]
      (when (= 1 value) (ellipse 
                          (cell-center-x x) 
                          (cell-center-y y)
                          cell-width
                          cell-height))))    

  ; Pac Man
  (fill 255 255 0)
  (ellipse (cell-center-x (first @(state :pacman-pos))) 
           (cell-center-y (second @(state :pacman-pos)))
           cell-width
           cell-height)  
  
	(let [partial-frame-gen (state :partial-frame)
        partial-frame-val (partial-frame-gen)]
    (when (= 0 partial-frame-val) 
      (reset! (state :pacman-pos) (move game-board @(state :pacman-pos) @(state :direction))))  
		(text (str partial-frame-val @(state :direction)) 20 60)))

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
		:size [(:width params) (:height params)]
		))
