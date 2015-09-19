(defproject cljminecraft "1.0.6-SNAPSHOT"
  :description "Clojure for Bukkit Minecraft"
  :dependencies [[org.clojure/clojure         "1.7.0"]
                 [org.clojure/tools.logging   "0.3.1"]
                 [org.clojure/tools.nrepl     "0.2.10"]
                 [org.bukkit/bukkit           "1.8.8-R0.1-SNAPSHOT"]
                 [org.spigotmc/spigot-api     "1.8.8-R0.1-SNAPSHOT"]
                 [clojure-complete            "0.2.4"]
                 [cheshire                    "5.5.0"]
                 [org.reflections/reflections "0.9.9-RC1"]
                 [cider/cider-nrepl           "0.9.1"]
                 [org.tcrawley/dynapath       "0.2.3"]
                 [lein-light-nrepl            "0.2.0"]]
  :profiles {:dev {:dependencies []}}
  :javac-options [ "-d" "classes/" "-source" "1.6" "-target" "1.6"]
  :java-source-paths ["javasrc"]
  :uberjar-exclusions [#"(org|com|gnu)[/](bukkit|avaje|yaml|getspout|json|trove|spigotmc)[/](.*)"
                       #"com[/]google[/]common[/](.*)"
                       #"org[/]apache[/]commons[/](.*)"
                       #"javax[/]persistence[/](.*)"
                       #"net[/]sf[/]cglib[/](.*)"]
  :repositories [["spigot.snapshots" "https://hub.spigotmc.org/nexus/content/repositories/snapshots/"]]
  )
