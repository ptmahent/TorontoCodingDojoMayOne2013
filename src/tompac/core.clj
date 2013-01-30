(ns tompac.core
  (:use quil.core quil.helpers.seqs)
  (:import java.awt.event.KeyEvent)
  (:gen-class))

(defn setup []
	(smooth)
	(frame-rate 4)
	(set-state! 
			:message (atom "START")
			:sequence (seq->stream (steps))))

(defn draw []
	(background 0)
	(let [sequencegen (state :sequence)
        sequenceval (sequencegen)]
		(text (str sequenceval @(state :message)) 20 60)))

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
