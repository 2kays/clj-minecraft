(ns cljminecraft.structure-apply
  (:require [cljminecraft.structure :as s]
            [cljminecraft.bukkit :as bk]
            [cljminecraft.core :as core]
            [cljminecraft.items :as i])
  (:import [org.bukkit Location]))

(defn- apply-block
  [material-data location]
  (let [state (-> location .getBlock .getState)
        old-material-data (.getData state)]
    (.setType state (.getItemType material-data))
    (.setData state material-data)
    (.update state true false) ;force=true, applyPhysics=false
    old-material-data))

(defn apply-to-world
  [struct location]
  (s/map-with-pos struct
                  (fn [x y z new-material-data]
                    (if (nil? new-material-data)
                      nil
                      (let [newloc (-> (.clone location) (.add x y z))]
                        (prn (.getX newloc) (.getY newloc) (.getZ newloc))
                        (apply-block new-material-data newloc))))))

(defn insertion-point
  [location]
  (let [pt (atom (s/box 0 0 0))
        old-blocks (atom (s/box 0 0 0))]
    (add-watch pt :change (fn [key ref old new]
                            (bk/sync @core/clj-plugin
                                     (fn []
                                       (apply-to-world @old-blocks location)
                                       (reset! old-blocks
                                               (apply-to-world new location))))))
    pt))

(def test (insertion-point (Location. (bk/world-by-name "world") -227 65 166)))

(reset! test (s/box 1 1 1  :outline (i/get-material [:wood :oak])))

;; (def checker (s/box 10 10 10))
;; (def checker (s/map-with-pos checker (fn [x y z b] (if (= 1 (mod (+ x y z) 2))
;;                                                      b (i/get-material :wood)))))

;; (def backup  (bk/sync @core/clj-plugin (fn []
;;                                          (apply-to-world checker
;;                                                          (Location. (bk/world-by-name "world") -247 72 181)))))

;; (bk/sync @core/clj-plugin
;;          #(prn  backup))

;; (bk/sync @core/clj-plugin (fn [] (apply-to-world backup (Location. (bk/world-by-name "world") -247 72 181))))
