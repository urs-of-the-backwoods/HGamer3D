import LMH 0xd632bb5447a6c93c

struct EngineConfig {
	headless : Bool; # run without graphics output
	flushGPU : Bool; 
	threads : Bool;  # multi threading enabled
	forceGL2 : Bool; # only GL2 mode, no GL3
}

enum LogLevel {
	warning;
	info;
	debug;
}

struct Logging {
	logLevel : LogLevel;
	quietLogging : Bool;
	logFileName : Text;
}

struct WindowG3D {
    width : Int32;
    height : Int32;
    borderless : Bool;
    fullScreen : Bool;
    resizable : Bool;
}

struct GraphicsQuality {
    shadow : LMH;      # Shadow Quality
    material : LMH;    # Material Quality
    texture : LMH;     # Texture Quality
    multisample : LMH; # Multisampling Quality
}

struct Graphics3DConfig {
	engine : EngineConfig;
	quality : GraphicsQuality;
	logging : Logging;
	window : WindowG3D;
} 

id64 Graphics3DConfig = 0x0884eb62b6674bff


