(ns cljminecraft.structure-test
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
  (is (= (+ 101 201 301 0)
         (-> (s/box 3 3 3 :fill 0)
             (s/move 100 200 300)
             (s/map-with-pos +)
             (s/at 101 201 301)))))

(deftest merge-any
  (is (= (#'s/merge-any 3 3 [1 2 3] 5 3 [-3 -4 -5] nil vector)
         [[1 nil] [2 nil] [3 -3] [nil -4] [nil -5]])))

(deftest box-hollow
  (is (= (s/box 3 3 3 :outline 1)
         (s/from-str {\1 1}
                     ["111"
                      "111"
                      "111"]
                     ["111"
                      "1 1"
                      "111"]
                     ["111"
                      "111"
                      "111"]))))

(deftest merge-partial
  (is (= (#'s/merge-x 3 3 [1 2 3] 5 3 [-3 -4 -5])
         [1 2 -3 -4 -5]))
  (is (= (#'s/merge-yzx (s/box 3 3 3 :fill 1)
                        (-> (s/box 3 3 3 :fill 1) (s/move 3 0 0)))
         (s/box 6 3 3 :fill 1))))

(deftest mix
  (is (= (s/mix (s/box 3 3 3 :outline 1)
                (-> (s/box 3 1 1 :fill 2) (s/move 2 1 2)))
         (s/from-str {\1 1 \2 2}
                     ["111  "
                      "111  "
                      "111  "]
                     ["111  "
                      "1 1  "
                      "11222"]
                     ["111  "
                      "111  "
                      "111  "])))
  (is (= (s/mix (-> (s/box 1 2 1 :fill 1) (s/move 1 0 0))
                (s/box 2 1 1 :fill 2))
         (s/from-str {\1 1 \2 2}
                     [" 1"]
                     ["22"]))))

(deftest rect-data
  (is (= (#'s/rect-data 3 1 :1 :1)
         [[:1] [:1] [:1]])))

(deftest box-small
  (is (= (s/box 3 1 1 :fill 1)
         (s/from-str {\1 1} ["111"])))
  (is (= (s/box 3 1 1 :outline 1)
         (s/from-str {\1 1} ["111"])))
  (is (= (s/box 1 1 1 :outline 1)
         (s/from-str {\1 1} ["1"])))
  (is (= (s/box 2 2 2 :outline 1)
         (s/from-str {\1 1} ["11" "11"] ["11" "11"])))
  (is (= (s/box 1 1 3 :outline 1)
         (s/from-str {\1 1} ["1" "1" "1"]))))

(deftest stack
  (is (= (s/mix (s/box 3 2 3 :fill 1)
                (-> (s/box 2 2 2 :fill 2) (s/move 0 2 0)))
         (s/stack :y
                  (s/box 3 2 3 :fill 1)
                  (s/box 2 2 2 :fill 2)))))
