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

#include "CEGUI.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"


HG3DEventController::HG3DEventController()
{
	CEGUI::ScriptModule *pModule = new HG3DEventModule(this);
	CEGUI::System::getSingleton().setScriptingModule(pModule) ;
}

HG3DEventController::~HG3DEventController()
{
}

void HG3DEventController::pushEvent(CEGUI::String name, CEGUI::String sender, CEGUI::Window* window)
{
	EvtCtrlEvent event;
	event.name = name;
	event.sender = sender;
	event.window = window;
	evtList.push_back(event);
}

bool HG3DEventController::eventsAvailable()
{
	return !evtList.empty();
}

void HG3DEventController::popEvent(CEGUI::String& name, CEGUI::String& sender, CEGUI::Window** window)
{
	EvtCtrlEvent event = evtList.front();
	evtList.pop_front();
	name = event.name;
	sender = event.sender;
	*window = event.window;
}

