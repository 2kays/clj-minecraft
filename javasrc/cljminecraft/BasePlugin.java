package cljminecraft;

import java.io.*;
import java.net.*;
import java.nio.charset.*;
import java.security.*;
import java.util.*;
import java.util.logging.*;

import org.bukkit.*;
import org.bukkit.command.*;
import org.bukkit.plugin.*;
import org.bukkit.plugin.java.*;





public abstract class BasePlugin extends JavaPlugin{
	
	public final static Charset UTF8 = Charset.forName("UTF-8");
	
	private final static Logger logger=Bukkit.getLogger();//it would've been the same instance across both main and child plugins
//	private final static Logger logger=Logger.getLogger( "Minecraft" );//this is equivalent to above
	
	//true if onEnable was successful, false or null(not found) if onEnable failed or was never executed
	private Boolean successfullyEnabled=null;//each plugin will have one of these
	
	static {//static initializer block
		boolean asserts=false;
		assert (true == (asserts=true));
		
		_info( "assertions are "+(!asserts?"NOT ":"")+"enabled"+
				(!asserts?" (to enable pass jvm option -ea when starting bukkit)":""));
	
		
		//this should only be executed for cljminecraft(the main not any children) plugin, and it is so if children have a depend on cljminecraft
		//bukkit will then make sure cljminecraft is loaded before them
		
		//one time in bukkit lifetime(right?) we set *loader* to the classloader which applies to any future clojure scripts loads
		ClassLoader previous = Thread.currentThread().getContextClassLoader();
		final ClassLoader parentClassLoader = ClojurePlugin.class.getClassLoader();
		Thread.currentThread().setContextClassLoader(parentClassLoader);
		try {
			//this happens only once when ClojurePlugin.class gets loaded
			_info("!!!!!!!!!!!!!First time clojure init!!");
			System.out.flush();
			
			clojure.lang.DynamicClassLoader newCL = (clojure.lang.DynamicClassLoader)AccessController.doPrivileged( new PrivilegedAction() {
				@Override
				public Object run() {
					assert parentClassLoader == ClojurePlugin.class.getClassLoader();
					assert this.getClass().getClassLoader() == ClojurePlugin.class.getClassLoader();//even though "this" is different
					return new clojure.lang.DynamicClassLoader( parentClassLoader );
				}
			} );
			clojure.lang.Var.pushThreadBindings( clojure.lang.RT.map( clojure.lang.Compiler.LOADER, newCL) );//so this variant is the one
//			System.err.println(clojure.lang.RT.CLOJURE_NS);
//			clojure.lang.Var.intern(clojure.lang.RT.CLOJURE_NS,//just as I thought this variant won't work 
//				Symbol.intern("clojure.lang.Compiler/LOADER")//can't intern namespace qualified symbol
////				clojure.lang.Compiler.LOADER.sym this is actually null because there's no counterpart in clojure? :O I thought it was *loader*
//				, newCL, true);
//			clojure.lang.RT.WARN_ON_REFLECTION;
			//XXX: turn on reflection warnings for all plugins (maybe add/override this in each config.yml
			//XXX: does the above make sense? not sure how
			//no: maybe specify namespace to that *var* - can't intern namespace qualified symbol(I predict)
			clojure.lang.Var.intern(clojure.lang.RT.CLOJURE_NS, 
				clojure.lang.Symbol.intern("*warn-on-reflection*")
				//there's no accessible java field from which to get the symbol directly (they are non-public but there in RT nd Compiler classes)
				, clojure.lang.RT.F, true);
			//the above is equivalent to clojure code: (set! *warn-on-reflection* true)
		}finally{
			Thread.currentThread().setContextClassLoader(previous);
		}
	}
	
	@Override
	public final void onLoad() {
		URL jarURL;
		// XXX: executes once for each plugin TODO: investigate what happens if plugman unload and load is used AND server `reload`
		//main concern is if the url already exists is it re-added? if not then is it re-ordered /moved at beginning or end?
		try {
			jarURL = this.getFile().toURI().toURL();
		} catch ( MalformedURLException e ) {
			throw new RuntimeException( "should never happen", e );
		}
		
		info( "loaded jar: " + jarURL );
		assert clojure.lang.Compiler.LOADER.isBound();
		( (clojure.lang.DynamicClassLoader)clojure.lang.Compiler.LOADER.deref() ).addURL( jarURL );
	}
	
	public BasePlugin() {
		super();
		//constructor
		info("CONSTRUCTOR");//for "+this.getFile().getAbsoluteFile()); these aren't yet set
		//we don't know yet for which plugin we got constructed
		//XXX: an instance is created of this class for every child plugin (including the main one) 
		//TODO: maybe add a test to make sure this didn't change in the future
	}
	
	public static void showClassPath(String prefix, ClassLoader cl){
		_info("=="+prefix+"== For classloader "+cl+" ----------");
		_info(getClassPath(cl));
		_info("=="+prefix+"== ----END---"+cl+" ----------");
	}
	
	
	public final static String getClassPath() {
		return getClassPath(Thread.currentThread().getContextClassLoader());
	}
	
	public final static String getClassPath(ClassLoader cl) {
		URL[] urls = ((URLClassLoader)cl).getURLs();
		String cp ="{";
		
		int max = urls.length-1;
		if (max>=0){
			cp+=" ";
		}
		for ( int i = 0; i <= max; i++ ) {
			URL url = urls[i];
        	try {
				cp+= url.toURI().toString();
				if(i != max) {
					cp+=", ";
				}else {
					cp+=" ";
				}
			} catch ( URISyntaxException use ) {
				use.printStackTrace();
				throw new RuntimeException(use);
			}
        }
        cp+="}";
        return cp;
	}
	
	public final void severe(String msg) {
		info(ChatColor.RED+"[SEVERE] "+ChatColor.RESET+msg);//because colored won't show [SEVERE] only [INFO] level msgs
	}
	
    public final void info(String msg) {
    	PluginDescriptionFile descFile = getDescription();
    	String pluginName = this.getClass().getName();
    	if (null != descFile) pluginName=descFile.getName();
    	tellConsole(ChatColor.GREEN+"["+pluginName+"]"+ChatColor.RESET+" "+msg);
    }
    
    public static final void _info(String msg) {
    	info(BasePlugin.class, msg);
    }
    
    public static final void info(Class cls, String msg) {
    	String className = cls.getName();//we won't know the difference if we're in main or child plugins (cljminecraft or memorystone) because they both use the same main class to start
    	tellConsole(ChatColor.DARK_AQUA+"["+className+"]"+ChatColor.RESET+" "+msg);//the color is likely never seen due to not inited color console sender
    }
    
    public final static void tellConsole( String msg ) {
		// nvm; find another way to display colored msgs in console without having [INFO] prefix
		// there's no other way it's done via ColouredConsoleSender of craftbukkit
		// there are only two ways: [INFO]+colors+suffix, or no colors + whichever suffix
		ConsoleCommandSender cons = Bukkit.getConsoleSender();
		if (null != cons) {
			cons.sendMessage( msg );// this will log with [INFO] level
		}else {
			logger.info(ChatColor.stripColor( msg));
		}
	}
    
    public void setSuccessfullyEnabled() {
		assert (null == successfullyEnabled) || (false == successfullyEnabled.booleanValue())
			:"should not have been already enabled without getting disabled first";
		
    	successfullyEnabled=Boolean.TRUE;
    }
    
    public void removeEnabledState() {
    	assert ((null == successfullyEnabled) || (true == successfullyEnabled.booleanValue()));
		successfullyEnabled=null;
    }
    
    public boolean wasSuccessfullyEnabled() {
    	return ((null != successfullyEnabled) && (true == successfullyEnabled.booleanValue()));
    }
    
    /**
     * if it doesn't return true, then stop() will not be called<br>
     * @return true if successfully enabled or false(or thrown exceptions) otherwise<br>
     */
    public abstract boolean start();//TODO: rename these or the clojure ones just so it's no confusion when reading code(because they have same name)
	
	
	/**
	 * called only if start() didn't fail (that is: it returned true and didn't throw exceptions)
	 * 
	 */
	public abstract void stop();

	
    //synchronized not needed because it's an instance method and each plugin has a different instance
	@Override
	public final void onEnable() {
		assert isEnabled() : "it should be set to enabled before this is called, by bukkit";
		
		if ( start() ) {
			setSuccessfullyEnabled();
		}
	}
	
	
	@Override
	public final void onDisable() {//called only when onEnable didn't fail (if we did the logic right)
    	assert !isEnabled():"it should be set to disabled before this is called, by bukkit";
    	
        String pluginName = getDescription().getName();
		if ( wasSuccessfullyEnabled() ) {
			// so it was enabled(successfully prior to this) then we can call to disable it
			try {
				stop();//return state unused
			} finally {
				// regardless of the failure to disable, we consider it disabled
				removeEnabledState();
			}
		} else {
			info( "did not attempt to disable " + pluginName
				+ " clojure Plugin because it wasn't successfully enabled previously" );
		}
    }
	
}
