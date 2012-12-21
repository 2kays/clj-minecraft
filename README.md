# clj-minecraft

clj-minecraft has two specific goals:

1) Open up support for other clojure plugins on Bukkit
2) Provide convenience functions to make writing plugins more
idiomatic to clojure.

The first objective is accomplished within the ClojurePlugin.java
class and the cljminecraft.core namespace. It will take care of
calling the [plugin].core/start function, passing in the Bukkit plugin
object for local plugin state handling.

The second objective is accomplished by the various other namespaces
in clj-minecraft. I'm keeping the structure fairly flat and concise so
that one can write idiomatic code for 80% of the plugin code.

VERY IMPORTANT: Please realize that this plugin is still in early dev,
API changes are fast and frequent so that I can settle on idiomatic
and simple code. I will try to update the changelog with every (major) push so
that you can follow what changes, but be aware that you need to
understand what changes are made and adjust your plugins accordingly.

Changelog:

21 December 2012:
 - Revamp the block action definitions by introducing a 'defaction' macro
 - Introduce cut, copy, paste and fork - still buggy though, especially for larger areas

19 December 2012:
 - get-material now always returns MaterialData and never Material for consistency
   - Note that this could lead to API breaks
 - Implement the first working version of the block drawing primitives
   - Similar idea to Logo, forward x, turn-right, forward x, extrude :up x
 - Playing with more things in the repl.clj scratchpatch
   - Noticed that you can send a block change to a player without changing the world.. I'll look into building the functionality to setup a 'virtual' paint mode in the block drawing. Either for all players, a list of players or a single player. It would need to track the virtual paint unless cleared so that it can resend it to players as required.
   
17 December 2012:
 - Implement entity searching function
 - Drop item function
 - Couple of world effect helper functions
 - An initial repl.clj as a scratchpatch

14 December 2012:
 - API Breaking Change: ev/event needs to now reference eventname as "player.player-interact" string instead of symbol.
 - Implement material handling types properly
   - You can now specify [:wood :jungle :north] as a material key for (get-material)
   - Supports everything down to [:mushroom false :north] for a non-stem north-painted mushroom block
 - Command Tab completion support for event types and entity types
 - Add 'spawnentity' and 'addevent' commands to showcase tabcompletion.
 - Add 'find-entity' function in entity.clj - can use that to lookup the kinds of entities
 - Tweak the event macro to be a simple function and the register-event to do the actual legwork.
 - Add 'find-event' and 'describe-event' for making events easier to poke at from the REPL
 - Moved the materials to items.clj
 - The item-stack function in items.clj uses get-material
   - This makes creating a specific itemstack straightforward and consistent (item-stack [:wood :jungle] 2)
 - Tweak the recipes to use get-material, to make recipe material definitions very precise
 - Add some class helpers in util.clj

09 December 2012:
 - Implement first version of recipe wrapper functions
   - Support for both shaped and unshaped recipes from the same function.
   - Doesn't support more complex materials, like Wood (that has type) yet.

08 December 2012:
 - Finished work on Commands

07 December 2012:
 - Lots of work on the command abstractions:
   - Common type autocompletion
   - Hardcoded list autocompletion
   - Function result autocompleion
   - On command dispatch, convert to known types before calling command function
   - NOTE: The command autocompletion and type conversion will change slightly soon to allow for a more customized open system where plugins can contribute with their own defmulti's
 - Implemented the /repl start|stop [port] command

## Usage

To build this plugin, simply clone and call 'lein uberjar' in the
project root (using lein 2). This will create a *-standalone.jar in the
target folder, which you can copy to the CraftBukkit/plugins folder.

NOTE that currently there is an issue with the uberjarring that
includes the Bukkit API classes - this is not ideal and needs to be fixed.

To create another clojure mod against the clj-minecraft plugin, have a
look at http://github.com/CmdrDats/clj-memorystone for an example.

In a nutshell, create a new ordinary clojure plugin using 'lein new',
then add the following to your project.clj:

```clojure
:dev-dependencies [[org.bukkit/bukkit "1.4.5-R0.3-SNAPSHOT"]
                   [cljminecraft "1.0.1-SNAPSHOT"]
                   [org.clojure/clojure "1.4.0"]
                   [org.clojure/tools.logging "0.2.3"]]
:repositories [["bukkit.snapshots" "http://repo.bukkit.org/content/repositories/snapshots"]]
```

Then create a plugin.yml file and include this content:

```
name: projectname
main: cljminecraft.ClojurePlugin
version: 1.0.0
website: project-url
author: author-name
description: plugin-description
depend: [cljminecraft]
class-loader-of: cljminecraft
```

Note that the depend and class-loader-of are very important.

Once you have done this, create a file /src/[projectname]/core.clj -
your [projectname] has to match exactly what you put in your
plugin.yml. No dashes, underscores of fancynesses.

Add the following to your core.clj:

```clojure
(ns pluginname.core
  (:require [cljminecraft.events :as ev])
  (:require [cljminecraft.player :as pl])
  (:require [cljminecraft.bukkit :as bk])
  (:require [cljminecraft.logging :as log])
  (:require [cljminecraft.files :as files]))

(defn block-break [ev]
  (pl/send-msg ev "You broke something! oh dear."))
  
(defn events
  []
  [(ev/event block.block-break #'block-break)])
   
(defn start [plugin]
  (ev/register-eventlist plugin (events)))  
```

Then run 'lein jar' from the command line on your project, copy the
target/*.jar into your CraftBukkit/plugins folder, start the server,
join and break something!

At this point, nRepl would have started, by default on the 4005 port -
you can hook into this with whatever editor supports nRepl so that you
can livecode, inspect the world and push new code for your plugin
across. This is a highly recommended way of building your plugins!

## Contributions

A huge thanks to aiscott and basicsensei for their contributions to clj-minecraft!

Please feel free to fork and make pull requests if you want to contribute,
I love code contributions - it makes the whole project that much more well rounded.
On that note, we're desperately needing documentation, so if you're keen to contribute
to the wiki, let me know.

## License

Copyright (C) 2012 Deon Moolman

Distributed under the Eclipse Public License, the same as Clojure.
