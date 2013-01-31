(ns tompac.core
  (:use quil.core quil.helpers.seqs)
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

(defn pacman-move-left [state]
  (if (= (-> state :pac-man :x) 0) state 
  (update-in state [:pac-man :x]  dec)) )

(defn can-move? [pac-man board size direction]
  (when (= 0 
    (get board 
      (inc 
        (+  (* size (:y pac-man))
            (:x pac-man))))
    true
)))

(defn board [state]
  (-> state :board :map))

(defn pacman-move-right [state]
  (let [pac-man (:pac-man state)
        board (board state)
        size (-> state :board :size)
    ]
    (println "can move?" 
      (can-move? pac-man board size :right))

  (if (= (-> state :pac-man :x) (-> state :board :right))
    (assoc-in state [:pac-man :x] 0)
    (update-in state [:pac-man :x] inc))))
(defn pacman-move-up [state]
  (if (= (-> state :pac-man :y) 0) state 
  (update-in state [:pac-man :y]  dec)) )
(defn pacman-move-down [state]
  (if (= (-> state :pac-man :y) 0) state 
  (update-in state [:pac-man :y]  inc)) )

(defn pacman-move [statemap direction]
  (case direction
    :left (pacman-move-left statemap)
    :right (pacman-move-right statemap)
    :up (pacman-move-up statemap)
    :down (pacman-move-down statemap))

) 
   



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
