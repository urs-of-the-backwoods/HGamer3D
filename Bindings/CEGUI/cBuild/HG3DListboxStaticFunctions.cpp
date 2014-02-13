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

// HG3DEventStaticFunctions.cpp
// a separate Scripting Event Handler, to forward GUI Events to Haskell

#include "CEGUI/CEGUI.h"
#include "HG3DListboxStaticFunctions.h"

void HG3DListboxStaticFunctions::listboxAddItem(CEGUI::Listbox * listbox, const CEGUI::String & itemString)
{
	CEGUI::ListboxTextItem* ti = new CEGUI::ListboxTextItem(itemString, 0, NULL, false, true);
	ti->setSelectionBrushImage("Vanilla-Images", "GenericBrush");
	ti->setTextColours(CEGUI::colour(0.0, 0.0, 0.0));
	listbox->addItem(ti);
}

void HG3DListboxStaticFunctions::comboboxAddItem(CEGUI::Combobox * combobox, const CEGUI::String & itemString)
{
	CEGUI::ListboxTextItem* ti = new CEGUI::ListboxTextItem(itemString, 0, NULL, false, true);
	ti->setSelectionBrushImage("Vanilla-Images", "GenericBrush");
	ti->setTextColours(CEGUI::colour(0.0, 0.0, 0.0));
	combobox->addItem(ti);
}

