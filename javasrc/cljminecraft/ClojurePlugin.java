package cljminecraft;
import org.bukkit.*;
import org.bukkit.command.*;
import org.bukkit.plugin.*;
import org.bukkit.plugin.java.JavaPlugin;

import clojure.lang.*;

import java.io.*;
import java.lang.ClassLoader;
import java.net.*;
import java.util.*;
import java.util.logging.*;

/**
 * an instance of this class is create for every plugin (including the main cljminecraft one) that depends on cljminecraft, because
 * it will have to have in its plugin.yml the following:<br>
 * "main: cljminecraft.ClojurePlugin"
 *
 */
public class ClojurePlugin extends BasePlugin {
	
	private final static String selfPluginName="cljminecraft";
	private final static String selfCoreScript="cljminecraft.core";
	private final static String selfEnableFunction="on-enable";
	private final static String selfDisableFunction="on-disable";
	
	
	private void showClassPath(String prefix, ClassLoader cl){
		System.out.println("=="+prefix+"== For classloader "+cl+" ----------");
        URL[] urls = ((URLClassLoader)cl).getURLs();
 
        for(URL url: urls){
        	System.out.println(url.getPath());//getFile());
        	try {
				System.out.println(url.toURI());
			} catch ( URISyntaxException e ) {
				e.printStackTrace();
			}
        }
        System.out.println("=="+prefix+"== ----END---"+cl+" ----------");
	}
	
	//XXX: this works for cljminecraft plugin or for any child plugins having "class-loader-of: cljminecraft" in their plugin.yml
	//but if that's satisfied then config.yml (inside the child's .jar) will be shadowed by cljminecraft(inside its .jar)
	//due to them using the same classloader (as CmdrDats said)
    private boolean loadClojureFile(String cljFile) {
//    	assert selfPluginName.equals( getDescription().getName() ):"you don't have to call this for other child plugins";
        try {
        	//note there is a clojure dynamic boolean var, maybe check it: *use-context-classloader*
        	showClassPath("1",ClassLoader.getSystemClassLoader());
        	 
			ClassLoader previous = Thread.currentThread().getContextClassLoader();
			
			showClassPath("2",previous);
			showClassPath("3", this.getClass().getClassLoader());
			

			System.out.println(this.getClass());
			Class<?> cls = Class.forName("cljminecraft.ClojurePlugin");
			System.out.println(cls);

			showClassPath("6", cls.getClassLoader());
			String path = "/S:/cb/plugins/memorystone-2.0.0-SNAPSHOT.jar";
			String urlPath = "jar:file://" + path + "!/";
//		    addURL (new URL (urlPath));
			URL urls [] = {new URL (urlPath)};
			URLClassLoader cl = new URLClassLoader(
//				((URLClassLoader)
//					this.getClass().getClassLoader()
//					cls.getClassLoader()
//					).getURLs()
					urls
					,cls.getClassLoader()
					);
			
			
			Thread.currentThread().setContextClassLoader(
//				new clojure.lang.DynamicClassLoader(previous)); 
//				new clojure.lang.DynamicClassLoader(this.getClass().getClassLoader()));
//				new clojure.lang.DynamicClassLoader(cl));
				cl);
//				cls.getClassLoader() );
//			Thread.currentThread().setContextClassLoader(cl);
			
			try {
				showClassPath("5", Thread.currentThread().getContextClassLoader());
				showClassPath("4", ClassLoader.getSystemClassLoader());
		        
//	        	Var.pushThreadBindings(RT.map(RT.USE_CONTEXT_CLASSLOADER, RT.T));
				
	        	System.out.println( "loading clojure file: " + cljFile );
				clojure.lang.RT.loadResourceScript( cljFile );
			} finally {
//				Var.popThreadBindings();
				Thread.currentThread().setContextClassLoader( previous );
			}
//			System.out.println( "loading clojure file: " + cljFile );
//			clojure.lang.RT.loadResourceScript( cljFile );
			return true;
		} catch ( Exception e ) {
			System.out.println( "Something broke setting up Clojure" );
			e.printStackTrace();
			return false;
		}
    }
    
	
	public final boolean loadClojureNameSpace( String ns ) {
		String cljFile = ns.replaceAll( "[.]", "/" ) + ".clj";
		return loadClojureFile( cljFile );
	}
    
    public Object invokeClojureFunction(String ns, String funcName) {
    	return clojure.lang.RT.var(ns, funcName).invoke(this);//passing the plugin instance as param
    }

    @Override
	public boolean start() {
    	
		String pluginName = getDescription().getName();
		
		boolean success = false;
		if ( selfPluginName.equals( pluginName ) ) {
			info( "Enabling main " + pluginName + " clojure Plugin" );
			success = loadClojureNameSpace(selfCoreScript);
		} else {
			info( "Enabling child " + pluginName + " clojure Plugin" );
			//the child plugin must have in plugin.yml: class-loader-of: cljminecraft
			//or the following will fail:
			success = loadClojureNameSpace(pluginName+".core");
		}

		invokeClojureFunction(selfCoreScript, selfEnableFunction );
		
		return success;
    }

    
    @Override
	public void stop() {//called only when onEnable didn't fail (if we did the logic right)
		String pluginName = getDescription().getName();
		if ( selfPluginName.equals( pluginName ) ) {
			info( "Disabling main " + pluginName + " clojure Plugin" );
		} else {
			info( "Disabling child " + pluginName + " clojure Plugin" );
		}
		invokeClojureFunction( selfCoreScript, selfDisableFunction );

    }
    

/*in plugin.yml of your clojure plugin which depends on cljminecraft, these are required:
 * 
 * main: cljminecraft.ClojurePlugin
 * depend: [cljminecraft]
 * 
 * and the name of your plugin(in your plugin.yml) should be the ns name of core.clj and core.clj should be the main script 
 * which includes the two methods start and stop which take plugin instance as parameter
 * 
  */  
}
