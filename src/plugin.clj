(ns cljminecraft.ClojurePlugin
  (:gen-class)  ;; :extends org.bukkit.plugin.java.JavaPlugin))

(defn -onEnable []
  (prn "onEnable lolka"))

(defn -onDisable []
  (prn "onDisable lalka"))
