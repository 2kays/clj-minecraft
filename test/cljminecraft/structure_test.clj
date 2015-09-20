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

(run-tests)
