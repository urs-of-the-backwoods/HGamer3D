/*
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2011 Peter Althainz

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

// HG3DEventStaticFunctions.h
// a separate Scripting Event Handler, to forward GUI Events to Haskell

#include "CEGUI/CEGUI.h"

class HG3DWindowStaticFunctions
{
 public:
 
	// functions, downcast to specialized windows, use with care :-)
	static CEGUI::PushButton* castWindowToPushButton(CEGUI::Window * window);
	static CEGUI::Listbox* castWindowToListbox(CEGUI::Window * window);
	static CEGUI::Combobox* castWindowToCombobox(CEGUI::Window * window);
	//	static CEGUI::Checkbox* castWindowToCheckbox(CEGUI::Window * window);
	static CEGUI::RadioButton* castWindowToRadioButton(CEGUI::Window * window);
	static CEGUI::Editbox* castWindowToEditbox(CEGUI::Window * window);
	static CEGUI::MultiLineEditbox* castWindowToMultiLineEditbox(CEGUI::Window * window);
	static CEGUI::FrameWindow* castWindowToFrameWindow(CEGUI::Window * window);
	static CEGUI::ProgressBar* castWindowToProgressBar(CEGUI::Window * window);
	static CEGUI::Slider* castWindowToSlider(CEGUI::Window * window);
	static CEGUI::Spinner* castWindowToSpinner(CEGUI::Window * window);
	static CEGUI::MultiColumnList* castWindowToMultiColumnList(CEGUI::Window * window);

	// utility functions for UDims and UVectors
	static float udScale( const CEGUI::UDim* ud);
	static float udOffset( const CEGUI::UDim* ud);
	static CEGUI::UDim* v2X( const CEGUI::UVector2* uv2);
	static CEGUI::UDim* v2Y( const CEGUI::UVector2* uv2);

	static CEGUI::UDim* getWindowWidth(const CEGUI::Window* window);
	static CEGUI::UDim* getWindowHeight(const CEGUI::Window* window);

	// utility function to set new window positions
        static void setNewWindowSize(CEGUI::System* system, float width, float height);

	// utility functions to get and set margin (only one parameter)
        static CEGUI::UDim* getWindowMargin(const CEGUI::Window* window);
        static void setWindowMargin (CEGUI::Window* window, const CEGUI::UDim* margin);
};

