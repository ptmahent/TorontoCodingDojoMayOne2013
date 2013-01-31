(ns tompac.core-test
  (:use clojure.test tompac.core quil.core quil.helpers.seqs))


(deftest pacman-move-test
  (testing "Next pacman moves pacman to the right"
    (is (= {:pac-man {:x 21 :y 20}}
    	   (pacman-move {:pac-man {:x 20 :y 20}} :right)))
	(let [state {:pac-man {:x 0 :y 1} 
    	   		:board {:map [1 1
    	   				      0 1]
    	   			    :size 2
    	   			    }
    	   		}]

	(is (= {:pac-man {:x 0 :y 1}}
    	   (select-keys 
    	   	(pacman-move state :right)
    	   [:pac-man])))))

  (testing "Next pacman moves pacman to the right"
    (is (= {:pac-man {:x 20 :y 20}}
    	   (pacman-move {:pac-man {:x 19 :y 20}} :right))))

  (testing "Next pacman moves pacman up"
    (is (= {:pac-man {:x 20 :y 19}}
    	   (pacman-move {:pac-man {:x 20 :y 20}} :up))))

  (testing "Next pacman moves pacman down"
    (is (= {:pac-man {:x 20 :y 21}}
    	   (pacman-move {:pac-man {:x 20 :y 20}} :down))))

  (testing "Pacman stops when it hits the right wall"
  	(is (= {:pac-man {:x 20 :y 0}}
    	   (select-keys 
    	   	(pacman-move {:pac-man {:x 20 :y 0} :board {:right 20 }} :right)
    	   [:pac-man]))))

  (testing "Pacman stops when it hits the right wall"
  	(is (= {:pac-man {:x 21 :y 0}}
    	   (select-keys 
    	   	(pacman-move {:pac-man {:x 21 :y 0} :board {:right 21 }} :right)
    	   [:pac-man]))))

  (testing "Pacman should re-appear at the left end when going at the rightmost"
  	(is (= {:pac-man {:x 0 :y 0}}
  		(select-keys
  			(pacman-move {:pac-man {:x 20 :y 0} :board {:right 20}} :right)
  			[:pac-man]))))

  )

