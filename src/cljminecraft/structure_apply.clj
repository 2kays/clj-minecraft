(ns cljminecraft.structure-apply
  (:require [cljminecraft.structure :as s])
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
