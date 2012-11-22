package cljminecraft;
import org.bukkit.plugin.java.JavaPlugin;
import java.lang.ClassLoader;

public class ClojurePlugin extends JavaPlugin {
    public void onEnable(String ns, String enableFunction) {
        try {
            ClassLoader previous = Thread.currentThread().getContextClassLoader();
            Thread.currentThread().setContextClassLoader(this.getClass().getClassLoader());

            clojure.lang.RT.loadResourceScript(ns.replaceAll("[.]", "/")+".clj");
            clojure.lang.RT.var(ns, enableFunction).invoke(this);

            Thread.currentThread().setContextClassLoader(previous);
        } catch (Exception e) {
            System.out.println("Something broke setting up Clojure");
            e.printStackTrace();
        }
    }

    public void onEnable() {
        String name = getDescription().getName();
        System.out.println("Enabling "+name+" clojure Plugin");

        onEnable("cljminecraft.core", "on-enable");
        getServer().getPluginManager().registerEvents(new PluginListener (), this);
    }

    public void onDisable(String ns, String disableFunction) {
        clojure.lang.RT.var(ns, disableFunction).invoke(this);
    }

    public void onDisable() {
        String name = getDescription().getName();
        System.out.println("Disabling "+name+" clojure Plugin");
        if ("clj-minecraft".equals(name)) {
            onEnable("cljminecraft.core", "on-disable");
        } else {
            onEnable(name+".core", "disable-plugin");
        }
    }
}
