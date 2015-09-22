(ns cljminecraft.structure
  )

;; data: y z x
(defrecord Structure [offset-x offset-y offset-z size-x size-y size-z data])

(defn- structure [size-x size-y size-z data]
  (Structure. 0 0 0 size-x size-y size-z data))

(defn at
  "Returns block at position x, y, z"
  [struct x y z]
  (let [[ix iy iz] (for [[p key] [[x :offset-x]
                                  [y :offset-y]
                                  [z :offset-z]]]
                     (- p (key struct)))]
    (get-in struct [:data iy iz ix])))

(defn set-at
  "Sets block at specified position"
  [struct x y z value] ; fixme: offset
  (assoc-in struct [:data x y z] value))

(defn bounds
  "Returns (ax, ay, az, bx, by, bz) bounds of struct (coordinates of corners)"
  [struct]
  [(:offset-x struct)
   (:offset-y struct)
   (:offset-z struct)
   (+ (:size-x struct) (:offset-x struct))
   (+ (:size-y struct) (:offset-y struct))
   (+ (:size-z struct) (:offset-z struct))])

(defn- rect-data [sx sz fill outline]
  (let [first-and-last (into [] (repeat sx outline))
        fills          (repeat (- sx 2) fill)
        middle         (into [] (concat [outline]
                                        fills
                                        [outline]))]
    (prn first-and-last fills middle (> sz 1) sx sz)
    (into [] (if (> sz 1)
               (concat [first-and-last]
                       (repeat (- sz 2) middle)
                       [first-and-last])
               (repeat sz first-and-last)))))

(defn rect
  "Rectangle of size (sx, 1, sz) with fill :fill (default nil) and outline
  :outline (default nil). If outline is nil, there will be no outline, only
  fill."
  [sx sz & {:keys [fill outline]}]
  (structure sx 1 sz (rect-data sx sz fill (or outline fill))))

(defn box
  "Box of size (sx, sy, sz) centered at (0, 0, 0) with fill :fill (default nil)
  and outline :outline (default nil). If outline is nil, there will be no
  outline, only fill."
  [sx sy sz & {:keys [fill outline]}]
  (structure sx sy sz
             (let [e-outline      (or outline fill)
                   first-and-last (rect-data sx sz e-outline e-outline)
                   middle         (rect-data sx sz fill e-outline)]
               (into [] (if (> sy 1)
                          (concat [first-and-last]
                                  (repeat (- sy 2) middle)
                                  [first-and-last])
                          (repeat sy first-and-last))))))

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

(defn generate
  "Generates structure of size (sx, sy, sz) by calling supplied function f with
  arguments x, y, z"
  [sx sy sz f]
  (structure sx sy sz
             (into [] (for [y (range sy)]
                        (into [] (for [z (range sz)]
                                   (into [] (for [x (range sx)]
                                              (f x y z)))))))))

(defn map-with-pos
  "Maps function f with arguments (x y z block) over each block, returning new
  structure"
  [struct f]
  (let [enumerate (fn [data offset-key]
                    (map vector data (iterate inc (offset-key struct))))
        data
        (into [] (for [[ydata y] (enumerate (:data struct) :offset-y)]
                   (into [] (for [[zdata z] (enumerate ydata :offset-z)]
                              (into [] (for [[xdata x]
                                             (enumerate zdata :offset-x)]
                                         (f x y z xdata)))))))]
    (assoc struct :data data)))

(defn from-str
  "Creates structure from list of lists of strings and mappings from characters
  to blocks"
  [mappings & strings]
  (let [ysize (count strings)
        zsizes (into #{} (map count strings))
        zsize (if (= 1 (count zsizes))
                (first zsizes)
                (throw (IllegalArgumentException.
                        "Not all layers have the same length")))
        xsizes (into #{} (for [layer strings
                               row layer]
                           (count row)))
        xsize (if (= 1 (count xsizes))
                (first xsizes)
                (throw (IllegalArgumentException.
                        "Not all strings have the same length")))
        data (into []
                   (reverse
                    (into []
                          (for [layer strings]
                            (into []
                                  (for [row layer]
                                    (into []
                                          (map mappings row))))))))]
    (structure xsize ysize zsize data)))

(defn- merged-bounds
  [offset-a size-a offset-b size-b]
  (let [leftmost (min offset-a offset-b)
        rightmost (max (+ offset-a size-a) (+ offset-b size-b))]
    [leftmost (- rightmost leftmost)]))


(defn- nil-square
  [sx sy]
  (let [row (into [] (repeat sy nil))]
    (into [] (repeat sx row))))

;; TODO: use structural sharing of vectors to optimize speed/memory
(defn- merge-any
  [offset-a size-a a offset-b size-b b empty merge-fn]
  {:pre [(vector? a) (vector? b) (fn? merge-fn)]}
  (let [[offset-result
         size-result] (merged-bounds offset-a size-a offset-b size-b)
        left-pad-a (- offset-a offset-result)
        left-pad-b (- offset-b offset-result)
        right-pad-a (- size-result (+ left-pad-a size-a))
        right-pad-b (- size-result (+ left-pad-b size-b))]
    (into [] (map merge-fn
                  (concat (repeat left-pad-a empty)
                          a
                          (repeat right-pad-a empty))
                  (concat (repeat left-pad-b empty)
                          b
                          (repeat right-pad-b empty))))))

(defn- merge-x
  [offset-a size-a a offset-b size-b b]
  {:pre [(vector? a) (vector? b)]}
  (merge-any offset-a size-a a offset-b size-b b nil (fn [a b] (or b a))))

(defn- merge-zx
  [offset-a size-a offset-a-x size-a-x a offset-b size-b offset-b-x size-b-x b]
  (let [[offset-result size-result]
        (merged-bounds offset-a size-a offset-b size-b)]
    (merge-any offset-a size-a a offset-b size-b b
               (into [] (repeat size-result nil))
               (fn [a-x b-x] (merge-x offset-a-x size-a-x
                                      a-x
                                      offset-b-x size-b-x
                                      b-x)))))

(defn- merge-yzx
  [a b]
  (let [[offset-result-y size-result-y]
        (merged-bounds (:offset-y a) (:size-y a) (:offset-y b) (:size-y b))
        [offset-result-z size-result-z]
        (merged-bounds (:offset-z a) (:size-z a) (:offset-z b) (:size-z b))
        [offset-result-x size-result-x]
        (merged-bounds (:offset-x a) (:size-x a) (:offset-x b) (:size-x b))
        data
        (merge-any (:offset-y a) (:size-y a) (:data a)
                   (:offset-y b) (:size-y b) (:data b)
                   (nil-square size-result-z size-result-y)
                   (fn [a-zx b-zx] (merge-zx (:offset-z a) (:size-z a)
                                             (:offset-x a) (:size-x a)
                                             a-zx
                                             (:offset-z b) (:size-z b)
                                             (:offset-x b) (:size-x b)
                                             b-zx)))]
    (Structure. offset-result-x offset-result-y offset-result-z
                size-result-x size-result-y size-result-z
                data)))

(defn mix
  [struct & structs]
  (reduce merge-yzx struct structs))
