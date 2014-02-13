// WindowUtilsHG3D - special utilities


#include <Ogre.h>

// Linux CODE
// ----------

#ifdef __linux__ 

class WindowUtilsHG3D {
	
	public:
		static void getWindowTopLeft(Ogre::RenderWindow* rwind, int* top, int* bottom, int* left, int* right) {
		  unsigned int width, height, colourDepth;
		  int left2, top2;
		  rwind->getMetrics (width, height, colourDepth, left2, top2);
		  *left = left2;
		  *top = top2;
		  *bottom = (int)(top2 + height);
		  *right = (int)(left2 + width);
		};
		
		static void showCursor(bool fShow) {
		};
	
};


// Windows CODE
// ------------

#elif _WIN32

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

#else

// CODE if not Windows or Linux
// ----------------------------

#endif
