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

// HG3DEventController.h
// this class static methods are called from Haskell

#ifndef _HG3DEventController_INCLUDE
#define _HG3DEventController_INCLUDE

#include <list>
#include "CEGUI.h"

class HG3DEventController
{
public:

	class EvtCtrlEvent
	{
		public:
			CEGUI::String name;
			CEGUI::String sender; 
			CEGUI::Window* window;
	};
	
	std::list<EvtCtrlEvent> evtList;

	HG3DEventController();
	~HG3DEventController();
	void pushEvent(CEGUI::String name, CEGUI::String sender, CEGUI::Window* window);
	bool eventsAvailable();
	void popEvent(CEGUI::String& name, CEGUI::String& sender, CEGUI::Window** window);
};

#endif
