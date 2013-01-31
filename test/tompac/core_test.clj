(ns tompac.core-test
  (:use clojure.test tompac.core))

(deftest pacman-move-on-board-test
  (testing "Move pacman left on board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [1 0]  :left) [0 0])))
  (testing "Move pacman right on board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [0 0] :right) [1 0])))
  (testing "Move pacman up on board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [0 1]    :up) [0 0])))
  (testing "Move pacman down"
    (is (= (pacman-move [[0 0]
                         [0 0]] [0 0]  :down) [0 1]))))

(deftest pacman-move-wrap-around-board-test
  (testing "Move pacman left wrapping around board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [0 0]  :left) [1 0])))
  (testing "Move pacman right wrapping around board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [1 0] :right) [0 0])))
  (testing "Move pacman up wrapping around board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [0 0]    :up) [0 1])))
  (testing "Move pacman down wrapping around board"
    (is (= (pacman-move [[0 0]
                         [0 0]] [0 1]  :down) [0 0]))))
