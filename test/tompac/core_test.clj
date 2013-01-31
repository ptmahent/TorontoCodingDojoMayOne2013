(ns tompac.core-test
  (:use clojure.test tompac.core))

(deftest pacman-move-test
  (testing "Move pacman left"
    (is (= (pacman-move [1 1]  :left) [0 1])))
  (testing "Move pacman right"
    (is (= (pacman-move [1 1] :right) [2 1])))
  (testing "Move pacman up"
    (is (= (pacman-move [1 1]    :up) [1 0])))
  (testing "Move pacman down"
    (is (= (pacman-move [1 1]  :down) [1 2]))))

