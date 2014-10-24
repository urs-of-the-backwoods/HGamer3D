// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// 

// HeaderSDLVideo.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "SDL2DllDefines.h"
	#include "ClassPtr.h"
	#include "./SDL.h"
#include "HG3DUtilities.h"




// Get the number of video drivers compiled into SDL. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetNumVideoDrivers(int * result_c)
{
  int result_cpp;
  result_cpp = (SDL_GetNumVideoDrivers());
  *result_c = (int)result_cpp;
};

// Get the name of a built in video driver. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetVideoDriver(int index_c, char * result_c)
{
  int index_cpp = (int)index_c;
  const char * result_cpp;
  result_cpp = (SDL_GetVideoDriver(index_cpp));
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

// Initialize the video subsystem, optionally specifying a video driver. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_VideoInit(char * driver_name_c, int * result_c)
{
  const char * driver_name_cpp = (char*) driver_name_c;
  int result_cpp;
  result_cpp = (SDL_VideoInit(driver_name_cpp));
  *result_c = (int)result_cpp;
};

// Shuts down the video subsystem. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_VideoQuit()
{
  (SDL_VideoQuit());
};

// Returns the name of the currently initialized video driver. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetCurrentVideoDriver(char * result_c)
{
  const char * result_cpp;
  result_cpp = (SDL_GetCurrentVideoDriver());
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

// Returns the number of available video displays. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetNumVideoDisplays(int * result_c)
{
  int result_cpp;
  result_cpp = (SDL_GetNumVideoDisplays());
  *result_c = (int)result_cpp;
};

// Get the name of a display in UTF-8 encoding. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetDisplayName(int displayIndex_c, char * result_c)
{
  int displayIndex_cpp = (int)displayIndex_c;
  const char * result_cpp;
  result_cpp = (SDL_GetDisplayName(displayIndex_cpp));
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

// Returns the number of available display modes. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetNumDisplayModes(int displayIndex_c, int * result_c)
{
  int displayIndex_cpp = (int)displayIndex_c;
  int result_cpp;
  result_cpp = (SDL_GetNumDisplayModes(displayIndex_cpp));
  *result_c = (int)result_cpp;
};

// Get the display index associated with a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowDisplayIndex(void * window_c, int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int result_cpp;
  result_cpp = (SDL_GetWindowDisplayIndex(window_cpp));
  *result_c = (int)result_cpp;
};

// Get the pixel format associated with the window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowPixelFormat(void * window_c, unsigned int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  Uint32 result_cpp;
  result_cpp = (SDL_GetWindowPixelFormat(window_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Create a window with the specified position, dimensions, and flags. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_CreateWindow(char * title_c, int x_c, int y_c, int w_c, int h_c, unsigned int flags_c, void * * result_c)
{
  const char * title_cpp = (char*) title_c;
  int x_cpp = (int)x_c;
  int y_cpp = (int)y_c;
  int w_cpp = (int)w_c;
  int h_cpp = (int)h_c;
  Uint32 flags_cpp = (Uint32)flags_c;
  SDL_Window * result_cpp;
  result_cpp = (SDL_CreateWindow(title_cpp, x_cpp, y_cpp, w_cpp, h_cpp, flags_cpp));
  *result_c = (void *)result_cpp;
};

// Get the numeric ID of a window, for logging purposes. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowID(void * window_c, unsigned int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  Uint32 result_cpp;
  result_cpp = (SDL_GetWindowID(window_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Get a window from a stored ID, or NULL if it doesn't exist. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowFromID(unsigned int id_c, void * * result_c)
{
  Uint32 id_cpp = (Uint32)id_c;
  SDL_Window * result_cpp;
  result_cpp = (SDL_GetWindowFromID(id_cpp));
  *result_c = (void *)result_cpp;
};

// Get the window flags. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowFlags(void * window_c, unsigned int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  Uint32 result_cpp;
  result_cpp = (SDL_GetWindowFlags(window_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Set the title of a window, in UTF-8 format. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowTitle(void * window_c, char * title_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  const char * title_cpp = (char*) title_c;
  (SDL_SetWindowTitle(window_cpp, title_cpp));
};

// Get the title of a window, in UTF-8 format. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowTitle(void * window_c, char * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  const char * result_cpp;
  result_cpp = (SDL_GetWindowTitle(window_cpp));
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

// Set the position of a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowPosition(void * window_c, int x_c, int y_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int x_cpp = (int)x_c;
  int y_cpp = (int)y_c;
  (SDL_SetWindowPosition(window_cpp, x_cpp, y_cpp));
};

// Get the position of a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowPosition(void * window_c, int * x_c, int * y_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int x_cpp;
  int y_cpp;
  (SDL_GetWindowPosition(window_cpp, &x_cpp, &y_cpp));
  *x_c = (int)x_cpp;
  *y_c = (int)y_cpp;
};

// Set the size of a window's client area. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowSize(void * window_c, int w_c, int h_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int w_cpp = (int)w_c;
  int h_cpp = (int)h_c;
  (SDL_SetWindowSize(window_cpp, w_cpp, h_cpp));
};

// Get the size of a window's client area. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowSize(void * window_c, int * w_c, int * h_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int w_cpp;
  int h_cpp;
  (SDL_GetWindowSize(window_cpp, &w_cpp, &h_cpp));
  *w_c = (int)w_cpp;
  *h_c = (int)h_cpp;
};

// Set the minimum size of a window's client area. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowMinimumSize(void * window_c, int min_w_c, int min_h_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int min_w_cpp = (int)min_w_c;
  int min_h_cpp = (int)min_h_c;
  (SDL_SetWindowMinimumSize(window_cpp, min_w_cpp, min_h_cpp));
};

// Get the minimum size of a window's client area. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowMinimumSize(void * window_c, int * w_c, int * h_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int w_cpp;
  int h_cpp;
  (SDL_GetWindowMinimumSize(window_cpp, &w_cpp, &h_cpp));
  *w_c = (int)w_cpp;
  *h_c = (int)h_cpp;
};

// Set the maximum size of a window's client area. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowMaximumSize(void * window_c, int max_w_c, int max_h_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int max_w_cpp = (int)max_w_c;
  int max_h_cpp = (int)max_h_c;
  (SDL_SetWindowMaximumSize(window_cpp, max_w_cpp, max_h_cpp));
};

// Get the maximum size of a window's client area. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowMaximumSize(void * window_c, int * w_c, int * h_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int w_cpp;
  int h_cpp;
  (SDL_GetWindowMaximumSize(window_cpp, &w_cpp, &h_cpp));
  *w_c = (int)w_cpp;
  *h_c = (int)h_cpp;
};

// Show a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_ShowWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_ShowWindow(window_cpp));
};

// Hide a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_HideWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_HideWindow(window_cpp));
};

// Raise a window above other windows and set the input focus. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_RaiseWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_RaiseWindow(window_cpp));
};

// Make a window as large as possible. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_MaximizeWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_MaximizeWindow(window_cpp));
};

// Minimize a window to an iconic representation. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_MinimizeWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_MinimizeWindow(window_cpp));
};

// Restore the size and position of a minimized or maximized window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_RestoreWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_RestoreWindow(window_cpp));
};

// Set a window's fullscreen state. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowFullscreen(void * window_c, unsigned int flags_c, int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  Uint32 flags_cpp = (Uint32)flags_c;
  int result_cpp;
  result_cpp = (SDL_SetWindowFullscreen(window_cpp, flags_cpp));
  *result_c = (int)result_cpp;
};

// Copy the window surface to the screen. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_UpdateWindowSurface(void * window_c, int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int result_cpp;
  result_cpp = (SDL_UpdateWindowSurface(window_cpp));
  *result_c = (int)result_cpp;
};

// Set the brightness (gamma correction) for a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_SetWindowBrightness(void * window_c, float brightness_c, int * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  float brightness_cpp = (float)brightness_c;
  int result_cpp;
  result_cpp = (SDL_SetWindowBrightness(window_cpp, brightness_cpp));
  *result_c = (int)result_cpp;
};

// Get the brightness (gamma correction) for a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_GetWindowBrightness(void * window_c, float * result_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  float result_cpp;
  result_cpp = (SDL_GetWindowBrightness(window_cpp));
  *result_c = (float)result_cpp;
};

// Destroy a window. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_DestroyWindow(void * window_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  (SDL_DestroyWindow(window_cpp));
};

// Allow the screen to be blanked by a screensaver. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_EnableScreenSaver()
{
  (SDL_EnableScreenSaver());
};

// Prevent the screen from being blanked by a screensaver. 
extern "C" SDL2_LIB_EXPORT void fsdlvid_sdl_DisableScreenSaver()
{
  (SDL_DisableScreenSaver());
};

