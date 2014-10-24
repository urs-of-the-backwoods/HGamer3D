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

// HeaderSDLVideo.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_HeaderSDLVideo
#define _DEFINED_HG3D_HeaderSDLVideo

#include "ClassPtr.h"


// Get the number of video drivers compiled into SDL. 
void fsdlvid_sdl_GetNumVideoDrivers(int * result_c);

// Get the name of a built in video driver. 
void fsdlvid_sdl_GetVideoDriver(int index_c, char * result_c);

// Initialize the video subsystem, optionally specifying a video driver. 
void fsdlvid_sdl_VideoInit(char * driver_name_c, int * result_c);

// Shuts down the video subsystem. 
void fsdlvid_sdl_VideoQuit();

// Returns the name of the currently initialized video driver. 
void fsdlvid_sdl_GetCurrentVideoDriver(char * result_c);

// Returns the number of available video displays. 
void fsdlvid_sdl_GetNumVideoDisplays(int * result_c);

// Get the name of a display in UTF-8 encoding. 
void fsdlvid_sdl_GetDisplayName(int displayIndex_c, char * result_c);

// Returns the number of available display modes. 
void fsdlvid_sdl_GetNumDisplayModes(int displayIndex_c, int * result_c);

// Get the display index associated with a window. 
void fsdlvid_sdl_GetWindowDisplayIndex(void * window_c, int * result_c);

// Get the pixel format associated with the window. 
void fsdlvid_sdl_GetWindowPixelFormat(void * window_c, unsigned int * result_c);

// Create a window with the specified position, dimensions, and flags. 
void fsdlvid_sdl_CreateWindow(char * title_c, int x_c, int y_c, int w_c, int h_c, unsigned int flags_c, void * * result_c);

// Get the numeric ID of a window, for logging purposes. 
void fsdlvid_sdl_GetWindowID(void * window_c, unsigned int * result_c);

// Get a window from a stored ID, or NULL if it doesn't exist. 
void fsdlvid_sdl_GetWindowFromID(unsigned int id_c, void * * result_c);

// Get the window flags. 
void fsdlvid_sdl_GetWindowFlags(void * window_c, unsigned int * result_c);

// Set the title of a window, in UTF-8 format. 
void fsdlvid_sdl_SetWindowTitle(void * window_c, char * title_c);

// Get the title of a window, in UTF-8 format. 
void fsdlvid_sdl_GetWindowTitle(void * window_c, char * result_c);

// Set the position of a window. 
void fsdlvid_sdl_SetWindowPosition(void * window_c, int x_c, int y_c);

// Get the position of a window. 
void fsdlvid_sdl_GetWindowPosition(void * window_c, int * x_c, int * y_c);

// Set the size of a window's client area. 
void fsdlvid_sdl_SetWindowSize(void * window_c, int w_c, int h_c);

// Get the size of a window's client area. 
void fsdlvid_sdl_GetWindowSize(void * window_c, int * w_c, int * h_c);

// Set the minimum size of a window's client area. 
void fsdlvid_sdl_SetWindowMinimumSize(void * window_c, int min_w_c, int min_h_c);

// Get the minimum size of a window's client area. 
void fsdlvid_sdl_GetWindowMinimumSize(void * window_c, int * w_c, int * h_c);

// Set the maximum size of a window's client area. 
void fsdlvid_sdl_SetWindowMaximumSize(void * window_c, int max_w_c, int max_h_c);

// Get the maximum size of a window's client area. 
void fsdlvid_sdl_GetWindowMaximumSize(void * window_c, int * w_c, int * h_c);

// Show a window. 
void fsdlvid_sdl_ShowWindow(void * window_c);

// Hide a window. 
void fsdlvid_sdl_HideWindow(void * window_c);

// Raise a window above other windows and set the input focus. 
void fsdlvid_sdl_RaiseWindow(void * window_c);

// Make a window as large as possible. 
void fsdlvid_sdl_MaximizeWindow(void * window_c);

// Minimize a window to an iconic representation. 
void fsdlvid_sdl_MinimizeWindow(void * window_c);

// Restore the size and position of a minimized or maximized window. 
void fsdlvid_sdl_RestoreWindow(void * window_c);

// Set a window's fullscreen state. 
void fsdlvid_sdl_SetWindowFullscreen(void * window_c, unsigned int flags_c, int * result_c);

// Copy the window surface to the screen. 
void fsdlvid_sdl_UpdateWindowSurface(void * window_c, int * result_c);

// Set the brightness (gamma correction) for a window. 
void fsdlvid_sdl_SetWindowBrightness(void * window_c, float brightness_c, int * result_c);

// Get the brightness (gamma correction) for a window. 
void fsdlvid_sdl_GetWindowBrightness(void * window_c, float * result_c);

// Destroy a window. 
void fsdlvid_sdl_DestroyWindow(void * window_c);

// Allow the screen to be blanked by a screensaver. 
void fsdlvid_sdl_EnableScreenSaver();

// Prevent the screen from being blanked by a screensaver. 
void fsdlvid_sdl_DisableScreenSaver();

#endif 
