(ns cljminecraft.structure-apply
  (:require [cljminecraft.structure :as s])
  (:import [org.bukkit Location]))

(defn apply-to-world
  [struct location]
  (s/map-with-pos struct
                  (fn [x y z new-material-data]
                    (let [newloc (-> (.clone location) (.add x y z))
                          state (-> newloc .getBlock .getState)
                          old-material-data (.getData state)]
                      (.setType state (.getItemType new-material-data))
                      (.setData state new-material-data)
                      (.update state true false) ;force=true, applyPhysics=false
                      old-material-data))))
