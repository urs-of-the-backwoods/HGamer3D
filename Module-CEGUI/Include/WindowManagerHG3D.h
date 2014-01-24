// WindowManagerHG3D - special wm class to get the singleton without templates

#include <CEGUI.h>


class WindowManagerHG3D {
	
	public:
		CEGUI::WindowManager* pWmgr;
		
		WindowManagerHG3D() {pWmgr = CEGUI::WindowManager::getSingletonPtr();};
		~WindowManagerHG3D() {};
		
		static CEGUI::WindowManager* getSingleton() 
		{
			return CEGUI::WindowManager::getSingletonPtr();
		}
		
		CEGUI::Window* loadWindowLayoutHG3D(const CEGUI::String& filename, const CEGUI::String& name_prefix)
		{
			return pWmgr->loadWindowLayout(filename, name_prefix);
		};
	
};


