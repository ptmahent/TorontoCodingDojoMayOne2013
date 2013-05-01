(ns tompac.core
  (:use [quil.core] 
        [quil.helpers.seqs])
  (:import java.awt.event.KeyEvent)
  (:gen-class))

(defn board-width [board]
  (-> board first count))

(defn board-height [board]
  (-> board count))

(defn board-dim [board]
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
  (wrap (board-dim board) (next-pos pos direction)))

(defn board-at-pos [board pos]
  (let [xpos (first pos)
        ypos (second pos)
        row  (nth board ypos)]
    (nth row xpos)))

(defn open? [board pos direction] 
  (= 0 (board-at-pos board (move board pos direction))))

(defn move-if-open [board pos direction]
  (if 
    (open? board pos direction)
    (move board pos direction)
    pos))

(defn init-dots [board]
  (let [alldots (for [x (range 0 (board-width board))
                      y (range 0 (board-height board))]
                      [x y])]
  (set (filter #(= (board-at-pos board %) 0) alldots))))

(defn has-dot? [dots pos]
  (dots pos))

(def eat-dot disj)

;; Model up from here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics here on down


(defn pacman-pos [] @(state :pacman-pos))
(defn set-pacman-pos [pos]
  (reset! (state :pacman-pos) pos))

(defn pacman-dir [] @(state :pacman-direction))
(defn set-pacman-dir [direction]
  (reset! (state :pacman-direction) direction))

(defn set-dots [dots] 
  (reset! (state :dots) dots))


(defn dots [] @(state :dots))


(defn partial-frame [] ((state :partial-frame)))

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
                 [0 0 0 0 0 0 0 1 3 3 3 1 0 0 0 0 0 0 0]
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


(defn setup []
  (smooth)
  (frame-rate 60)
  (set-state!
      :pacman-pos (atom [9 15]) 
      :pacman-direction (atom :down)
      :dots (atom (init-dots game-board))
      :partial-frame (seq->stream (cycle-between 0 10))))
  

(def game-board-width (board-width game-board))
(def game-board-height (board-height game-board))

(def cell-width  (/ (:width  params) game-board-width))
(def cell-height (/ (:height params) game-board-height))

(def half-cell-width  (/ cell-width 2))
(def half-cell-height (/ cell-height 2))

(defn cell-top-x [x] (* x cell-width))
(defn cell-top-y [y] (* y cell-height))
(defn cell-center-x [x] (+ (cell-top-x x) half-cell-width))
(defn cell-center-y [y] (+ (cell-top-y y) half-cell-height))

(defn draw-rect [x y width height]
  (rect (cell-top-x x) (cell-top-y y) width height))

(defn draw-dot [x y]
  (fill 255 255 255)
  (ellipse (cell-center-x x) 
           (cell-center-y y)
           (/ cell-width 4)
           (/ cell-height 4)))

(defn draw-wall-brick [x y]
  (fill 0 0 255)
  (draw-rect x y cell-width cell-height))

(defn draw-ghosts-door [x y]
  (fill 255 255 255)
  (draw-rect x y cell-width (/ cell-height 4)))                                    
                                    
(defn draw []
	(background 0)
  
  ; Draw the Grid
  (doseq [[y row] (map-indexed vector game-board)]
    (doseq [[x value] (map-indexed vector row)]
      ;; condp #{:dot} #{:wall} #{:dot :ghost}
      (case 
        value
        0 (if ((dots) [x y]) (draw-dot x y))   
        1 (draw-wall-brick x y)
        2 (draw-ghosts-door x y)
        nil)))



  ; Draw Pac Man
  (fill 255 255 0)
  (ellipse (cell-center-x (first (pacman-pos))) 
           (cell-center-y (second (pacman-pos)))
           cell-width
           cell-height)  
  
  ; Move Pac Man
  (when
    (= 0 (partial-frame))
    (set-pacman-pos (move-if-open game-board (pacman-pos) (pacman-dir)))
    (set-dots (eat-dot (dots) (pacman-pos))))
  
  ; Draw some debugging text
  (text (str "DEBUG: " (pacman-dir) " " (partial-frame)) 20 60))
  
        
(defn direction-key-pressed [key-direction]
    (when 
      (open? game-board (pacman-pos) key-direction)
      (set-pacman-dir key-direction)))
        
(def key-directions {
  KeyEvent/VK_UP :up
  KeyEvent/VK_DOWN :down
  KeyEvent/VK_LEFT :left
  KeyEvent/VK_RIGHT :right
  \w :up
  \s :down
  \a :left
  \d :right})

(defn key-pressed []
  (let [raw-key (raw-key)
    the-key-code (key-code)
    the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)
    key-direction (get key-directions the-key-pressed nil)]
    (when key-direction 
      (direction-key-pressed key-direction))))

(defn -main [& args]
	(defsketch pacman 
		:title "pacman"
		:setup setup
		:draw draw
		:key-pressed key-pressed
		:size [(:width params) (:height params)]))
