// WindowManagerHG3D - special wm class to get the singleton without templates

#include <CEGUI.h>


class NoLogger : public CEGUI::Logger
{
	   void logEvent (const CEGUI::String&, CEGUI::LoggingLevel)
	   {

	   }
	   void setLogFilename(const CEGUI::String&, bool)
	   {      

	   }
};


class SystemHG3D {
	

	public:
	
		static CEGUI::Logger* createNoLogger() {
			return new NoLogger();
		}
		
		static CEGUI::DefaultResourceProvider* getDefaultResourceProvider(CEGUI::System* system) {
			return static_cast<CEGUI::DefaultResourceProvider*> (system->getResourceProvider());
		};
		static CEGUI::SchemeManager* getSchemeManagerSingleton() {
			return static_cast<CEGUI::SchemeManager*> (CEGUI::SchemeManager::getSingletonPtr());
		};
		static void schemeManagerCreate(CEGUI::SchemeManager* smgr, const char* scheme) {
			smgr->create(scheme);
		};
		static void fontManagerCreate(CEGUI::FontManager* fmgr, const char* font) {
			fmgr->create(font);
		};
		static CEGUI::FontManager* getFontManagerSingleton() {
			return static_cast<CEGUI::FontManager*> (CEGUI::FontManager::getSingletonPtr());
		};
		static CEGUI::Logger* getLoggerSingleton() {
			return static_cast<CEGUI::Logger*> (CEGUI::Logger::getSingletonPtr());
		};
	
};


