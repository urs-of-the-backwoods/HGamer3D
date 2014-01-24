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

// HG3DEventModule.cpp
// a separate Scripting Event Handler, to forward GUI Events to Haskell

#include "HG3DEventModule.h"
#include "HG3DCommandHandler.h"

HG3DEventModule::HG3DEventModule(HG3DEventController* ectrl)
{
	eventController = ectrl;
}


CEGUI::Event::Connection HG3DEventModule::subscribeEvent(
   CEGUI::EventSet*     target, 
   const CEGUI::String& name, 
   const CEGUI::String& subscriber_name)
{
 HG3DCommandHandler command(subscriber_name.c_str(), eventController) ;
 return target->subscribeEvent(name,CEGUI::Event::Subscriber(command)) ;
}

CEGUI::Event::Connection HG3DEventModule::subscribeEvent(
   CEGUI::EventSet*     target,
   const CEGUI::String& name,
   CEGUI::Event::Group  group,
   const CEGUI::String& subscriber_name)
{
 HG3DCommandHandler command(subscriber_name.c_str(), eventController) ;
 return target->subscribeEvent(name,group,CEGUI::Event::Subscriber(command)) ;
}

void HG3DEventModule::executeScriptFile(const CEGUI::String &filename, const CEGUI::String &resourceGroup)
{}

int HG3DEventModule::executeScriptGlobal(const CEGUI::String& function_name)
{
 return 0 ;
}


void HG3DEventModule::executeString(const CEGUI::String &str)
{}

bool HG3DEventModule::executeScriptedEventHandler(
   const CEGUI::String& handler_name,
   const CEGUI::EventArgs &e)
{
 return true ;
}

