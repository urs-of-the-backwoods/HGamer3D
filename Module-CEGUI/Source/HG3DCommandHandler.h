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

// HG3DCommandHandler.h
// Handles events of the Event Module

#ifndef _HG3DCommandHandler_INCLUDE
#define _HG3DCommandHandler_INCLUDE

#include "CEGUI.h"
#include "HG3DEventController.h"

class HG3DCommandHandler
 {
 public:
   
   HG3DCommandHandler(const std::string& name, HG3DEventController* ectrl);
   bool operator()(const CEGUI::EventArgs& args) const;
   std::string m_name ;
   HG3DEventController* eventController;
   
 };

#endif
