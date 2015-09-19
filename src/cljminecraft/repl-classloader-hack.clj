(ns cljminecraft.repl-classloader-hack
  (:require [dynapath.dynamic-classpath :as cp]))

;; Hack to make it work with custom Bukkit classloader
;; See:
;; https://github.com/clojure-emacs/cider-nrepl#with-jboss-asjboss-eapwildfly

(extend org.bukkit.plugin.java.PluginClassLoader
  cp/DynamicClasspath
  (assoc cp/base-readable-addable-classpath
         :classpath-urls #(seq (.getURLs %))
         :can-add? (constantly false)))
