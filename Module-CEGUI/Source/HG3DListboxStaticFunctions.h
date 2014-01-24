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

#include "CEGUI.h"

class HG3DListboxStaticFunctions
{
 public:
 
	static void listboxAddItem(CEGUI::Listbox * listbox, const CEGUI::String & itemString);
	static void comboboxAddItem(CEGUI::Combobox * combobox, const CEGUI::String & itemString);
};

