// This source file is part of HGamer3D
// (A project to enable 3D game development in Haskell)
// For the latest info, see http://www.hgamer3d.org
//
// (c) 2015 Peter Althainz
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

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "input.hpp"
#include "Urho3D/Input/InputEvents.h"

using namespace std;

extern "C" {
#include "interface.h"
}

InputEventHub::InputEventHub(Graphics3DSystem* g3ds)
: Object(g3ds->context)
{
	mouseEventF = NULL;
}

InputEventHub::~InputEventHub()
{
}

int InputEventHub::create(char* pdata, int len)
{
  SubscribeToEvent(E_MOUSEMOVE, HANDLER(InputEventHub, HandleMouseMove));
  SubscribeToEvent(E_MOUSEBUTTONUP, HANDLER(InputEventHub, HandleMouseButtonUp));
  return 0; 
}

void InputEventHub::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
        using namespace MouseMove;
        int x = eventData[P_DX].GetInt();
        int y = eventData[P_DY].GetInt();

	if (mouseEventF != NULL) {
		
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
                
		pk.pack_array(3);
		pk.pack(1);
		pk.pack(x);
		pk.pack(y);

		mouseEventF((void*)this, buffer.data(), buffer.size());
	}
}

void InputEventHub::HandleMouseButtonUp(StringHash eventType, VariantMap& eventData)
{
    std::cout << "Mouse Button Up Event" << std::endl;
}

void InputEventHub::registerMouseEvent(msgFP f)
{
  mouseEventF = f;
}


