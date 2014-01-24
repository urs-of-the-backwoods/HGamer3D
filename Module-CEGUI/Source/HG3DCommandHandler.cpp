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

// HG3DCommandHandler.cpp
// Handles events of the Event Module

#include "CEGUI.h"
#include <iostream>
#include "HG3DCommandHandler.h"

HG3DCommandHandler::HG3DCommandHandler(const std::string& name, HG3DEventController* ectrl)
: m_name(name), eventController(ectrl)
{}

bool HG3DCommandHandler::operator()(const CEGUI::EventArgs& args) const
{
 CEGUI::String sender = "";
 CEGUI::Window* window = NULL;
 
 const CEGUI::WindowEventArgs* argument = dynamic_cast<const CEGUI::WindowEventArgs*>(&args) ;
 if (argument)
 {
   // we have the window that triggered the event
   window = argument->window;
   sender = argument->window->getName();
 }
 
 eventController->pushEvent(m_name, sender, window);
 return true ;
}
