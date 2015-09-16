(ns cljminecraft.ClojurePlugin
  (:gen-class #_ :extends #_ org.bukkit.plugin.java.JavaPlugin))

(defn -onEnable [this]
  (prn "onEnable lolka"))

(defn -onDisable [this]
  (prn "onDisable lalka"))
