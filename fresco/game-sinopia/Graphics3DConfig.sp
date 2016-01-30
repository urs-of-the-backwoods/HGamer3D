namespace HGamer3D.Graphics3D.Config

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

enum QualityLMH {
	low;
	medium;
	high;
}

struct WindowG3D {
    width : Int;
    height : Int;
    borderless : Bool;
    fullScreen : Bool;
    resizable : Bool;
}

struct GraphicsQuality {
    shadow : QualityLMH;      # Shadow Quality
    material : QualityLMH;    # Material Quality
    texture : QualityLMH;     # Texture Quality
    multisample : QualityLMH; # Multisampling Quality
}

struct Graphics3DConfig {
	engine : EngineConfig;
	quality : GraphicsQuality;
	logging : Logging;
	window : Window;
} 

id64 Graphics3DConfig = 0x0884eb62b6674bff


