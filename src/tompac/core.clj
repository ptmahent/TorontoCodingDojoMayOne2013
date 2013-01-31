(ns tompac.core
  (:use [quil.core] 
        [quil.helpers.seqs]
        [incanter.core :only [matrix dim plus matrix-map]])
  (:import java.awt.event.KeyEvent)
  (:gen-class))


(defn setup []
	(smooth)
	(frame-rate 30)
	(set-state! 
			:message (atom "START")
			:sequence (seq->stream (steps))))

(defn draw []
	(background 0)

  (ellipse 100 100 20 20)
	(let [sequencegen (state :sequence)
        sequenceval (sequencegen)]
		(text (str sequenceval @(state :message)) 20 60)))

(def direction-factor {:left  [-1  0]
                       :right [ 1  0]
                       :up    [ 0 -1]
                       :down  [ 0  1]})

(defn pacman-move [board pos direction] 
   (let [board-matrix (matrix board)
         board-dimentions (dim board-matrix)
         direction-factor (direction direction-factor)
         board-dimension (if (contains? [:left :right] direction) 
                             (first board-dimentions)
                             (second board-dimentions))]
    (matrix-map (fn [x] (mod x board-dimension)) 
      (plus pos direction-factor))))

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
    (reset! (state :message) (str "KEY: " move))))

(defn -main [& args]
	(defsketch pacman 
		:title "pacman"
		:setup setup
		:draw draw
		:key-pressed key-press
		:size [320 200]
		))
