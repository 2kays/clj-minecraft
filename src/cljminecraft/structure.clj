(ns cljminecraft.structure
  )

;; data: y z x
(defrecord Structure [offset-x offset-y offset-z size-x size-y size-z data])

(defn at
  "Returns block at position x, y, z"
  [struct x y z]
  (let [[ix iy iz] (for [[p key] [[x :offset-x]
                                  [y :offset-y]
                                  [z :offset-z]]]
                     (- p (key struct)))]
    (-> struct :data (nth iy) (nth iz) (nth ix))))

(defn- structure [size-x size-y size-z data]
  (Structure. 0 0 0 size-x size-y size-z data))

(defn- rect-data [sx sz fill outline]
  (let [first-and-last (into [] (repeat sx outline))
        fills          (repeat (- sx 2) fill)
        middle         (into [] (concat [outline]
                                        fills
                                        [outline]))]
    (into [] (concat [first-and-last]
                     (repeat (- sz 2) middle)
                     [first-and-last]))))

(defn rect
  "Rectangle of size (sx, 1, sz) with fill :fill (default nil) and outline
  :outline (default nil)"
  [sx sz & {:keys [fill outline]}]
  (structure sx 1 sz (rect-data sx sz fill outline)))

(defn box
  "Box of size (sx, sy, sz) centered at (0, 0, 0) with fill :fill (default nil)
  and outline :outline (default nil)"
  [sx sy sz & {:keys [fill outline]}]
  (structure sx sy sz
             (let [first-and-last (rect-data sx sz outline outline)
                   middle         (rect-data sx sz fill outline)]
               (into [] (concat [first-and-last]
                                (repeat (- sy 2) middle)
                                [first-and-last])))))

(defn move
  "Moves structure by changing offset relatively by x, y, z"
  [struct x y z]
  (let [[newoff-x newoff-y newoff-z] (for [[offset-by prop] [[x :offset-x]
                                                             [y :offset-y]
                                                             [z :offset-z]]]
                                       (+ offset-by (prop struct)))]
    (Structure. newoff-x newoff-y newoff-z
                (:size-x struct) (:size-y struct) (:size-z struct)
                (:data struct))))

;(->  (box 5 5 5 :fill 1 :outline 0) (move 20 20 20) (move -5 -5 -1))
