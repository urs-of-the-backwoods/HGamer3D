// WindowUtilsHG3D - special utilities


#include <Ogre.h>
#include <windows.h>

class WindowUtilsHG3D {
	
	public:
		static void getWindowTopLeft(Ogre::RenderWindow* rwind, int* top, int* bottom, int* left, int* right) {
			HWND windowHnd = 0;
			RECT rect;
			rwind->getCustomAttribute("WINDOW", &windowHnd);
			GetWindowRect(windowHnd, &rect);
			*top = rect.top;
			*bottom = rect.bottom;
			*left = rect.left;
			*right = rect.right;
		};
		
		static void showCursor(bool fShow) {
			if (fShow) {
				ShowCursor(TRUE);
			} else {
				ShowCursor(FALSE);
			};
		};
	
};


