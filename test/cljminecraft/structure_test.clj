(ns cljminecraft.structure.test
  (:require [cljminecraft.structure :as s]
            [clojure.test :refer (deftest is run-tests)]))

(deftest box-move-and-at
  (is (= 1 (-> (s/box 5 5 5 :fill 0 :outline 1) (s/move 20 20 20) (s/at 20 20 20)))))

(deftest box-empty
  (s/box 0 0 0))

(deftest generate
  (let [sample-box (s/box 3 3 3 :fill 2 :outline 3)
        sample-gen (s/generate 3 3 3 (fn [x y z] (if (= [x y z] [1 1 1]) 2 3)))]
    (is (= sample-box sample-gen))))

(deftest map-with-pos
  (is (= (+ 101 201 301)
         (-> (s/box 3 3 3 :fill -1)
             (s/move 100 200 300)
             (s/map-with-pos +)
             (s/at 101 201 301)))))

(deftest merge-any
  (is (= (#'s/merge-any 3 3 [1 2 3] 5 3 [-3 -4 -5] nil vector)
         [[1 nil] [2 nil] [3 -3] [nil -4] [nil -5]])))

(deftest merge
  (is (= (#'s/merge-x 3 3 [1 2 3] 5 3 [-3 -4 -5])
         [1 2 -3 -4 -5]))
  (is (= (#'s/merge-yzx (s/box 3 3 3 :fill 1)
                        (-> (s/box 3 3 3 :fill 1) (s/move 3 0 0)))
         (s/box 6 3 3 :fill 1))))
