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


//
// Mouse
//

Mouse::Mouse(Graphics3DSystem* g3ds)
: Object(g3ds->context)
{
    input = new Input(g3ds->context);
	mouseEventF = NULL;
	visibleEventF = NULL;
}

Mouse::~Mouse()
{
  UnsubscribeFromEvent(E_MOUSEBUTTONUP);
  UnsubscribeFromEvent(E_MOUSEBUTTONDOWN);
  UnsubscribeFromEvent(E_MOUSEMOVE);
  UnsubscribeFromEvent(E_MOUSEWHEEL);
  UnsubscribeFromEvent(E_MOUSEVISIBLECHANGED);
  
  mouseEventF = NULL;
  visibleEventF = NULL;
  delete input;
}

int Mouse::create(char* pdata, int len)
{
  SubscribeToEvent(E_MOUSEBUTTONUP, HANDLER(Mouse, HandleMouseButtonUp));
  SubscribeToEvent(E_MOUSEBUTTONDOWN, HANDLER(Mouse, HandleMouseButtonDown));
  SubscribeToEvent(E_MOUSEMOVE, HANDLER(Mouse, HandleMouseMove));
  SubscribeToEvent(E_MOUSEWHEEL, HANDLER(Mouse, HandleMouseWheel));
  SubscribeToEvent(E_MOUSEVISIBLECHANGED, HANDLER(Mouse, HandleMouseVisibleChanged));
  
  msgMouse(pdata, len);
  return 0; 
}

int Mouse::msgMouse(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "mouse: " << obj << std::endl;
  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 2) return ERROR_TYPE_NOT_KNOWN;
  
  // set mouse mode
  if (obj.via.array.ptr[1].as<int>() == 1)
  {
      input->SetMouseMode(MM_ABSOLUTE);
  }
  else if (obj.via.array.ptr[1].as<int>() == 2)
  {
      input->SetMouseMode(MM_RELATIVE);
  }
  else if (obj.via.array.ptr[1].as<int>() == 3)
  {
      input->SetMouseMode(MM_WRAP);
  }
}
  
int Mouse::msgVisible(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "mouse-visible: " << obj << std::endl;
  
  if (obj.type != msgpack::type::BOOLEAN) return ERROR_TYPE_NOT_KNOWN;
  // set visibility
  input->SetMouseVisible(obj.as<bool>());       // true -> suppress event
  return 0;
}

void Mouse::HandleMouseButtonUp(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
		pk.pack_array(4);
		pk.pack(1);
		pk.pack_fix_int32(eventData[MouseButtonUp::P_BUTTON].GetInt());
		pk.pack_fix_int32(eventData[MouseButtonUp::P_BUTTONS].GetInt());
		pk.pack_fix_int32(eventData[MouseButtonUp::P_QUALIFIERS].GetInt());
		mouseEventF((void*)this, buffer.data(), buffer.size());
	}
}

void Mouse::HandleMouseButtonDown(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
		pk.pack_array(4);
		pk.pack(2);
		pk.pack_fix_int32(eventData[MouseButtonDown::P_BUTTON].GetInt());
		pk.pack_fix_int32(eventData[MouseButtonDown::P_BUTTONS].GetInt());
		pk.pack_fix_int32(eventData[MouseButtonDown::P_QUALIFIERS].GetInt());
		mouseEventF((void*)this, buffer.data(), buffer.size());
	}
}

void Mouse::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
	    int x = eventData[MouseMove::P_DX].GetInt();
		int y = eventData[MouseMove::P_DY].GetInt();
		pk.pack_array(7);
		pk.pack(3);
        if (input->IsMouseVisible()) {
            pk.pack_fix_int32(eventData[MouseMove::P_X].GetInt());
            pk.pack_fix_int32(eventData[MouseMove::P_Y].GetInt());
        } else {
            pk.pack_fix_int32(0);
            pk.pack_fix_int32(0);
        }
		pk.pack_fix_int32(eventData[MouseMove::P_DX].GetInt());
		pk.pack_fix_int32(eventData[MouseMove::P_DY].GetInt());
		pk.pack_fix_int32(eventData[MouseMove::P_BUTTONS].GetInt());
		pk.pack_fix_int32(eventData[MouseMove::P_QUALIFIERS].GetInt());
		mouseEventF((void*)this, buffer.data(), buffer.size());
	}
}

void Mouse::HandleMouseWheel(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
		pk.pack_array(4);
		pk.pack(4);
		pk.pack_fix_int32(eventData[MouseWheel::P_WHEEL].GetInt());
		pk.pack_fix_int32(eventData[MouseWheel::P_BUTTONS].GetInt());
		pk.pack_fix_int32(eventData[MouseWheel::P_QUALIFIERS].GetInt());
		mouseEventF((void*)this, buffer.data(), buffer.size());
	}
}

void Mouse::HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData)
{
	if (visibleEventF != NULL) {
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
		pk.pack(eventData[MouseVisibleChanged::P_VISIBLE].GetBool());
		visibleEventF((void*)this, buffer.data(), buffer.size());
	}
}

void Mouse::registerMouseEvent(msgFP f)
{
  mouseEventF = f;
}

void Mouse::registerVisibleEvent(msgFP f)
{
  visibleEventF = f;
}




//
// KeyEventHandler
//

KeyEventHandler::KeyEventHandler(Graphics3DSystem* g3ds)
: Object(g3ds->context)
{
	eventF = NULL;
    input = new Input(g3ds->context);
}

KeyEventHandler::~KeyEventHandler()
{
  UnsubscribeFromEvent(E_KEYUP);
  UnsubscribeFromEvent(E_KEYDOWN);
  eventF = NULL;
  delete input;
}

int KeyEventHandler::create(char* pdata, int len)
{
  SubscribeToEvent(E_KEYUP, HANDLER(KeyEventHandler, HandleKeyUp));
  SubscribeToEvent(E_KEYDOWN, HANDLER(KeyEventHandler, HandleKeyDown));
  return 0; 
}

void KeyEventHandler::HandleKeyUp(StringHash eventType, VariantMap& eventData)
{
	if (eventF != NULL) {
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
        pk.pack_array(4);
        pk.pack(1);
        int sc = eventData[KeyUp::P_SCANCODE].GetInt();
        pk.pack_fix_int32(eventData[KeyUp::P_KEY].GetInt());
        pk.pack_fix_int32(sc);
        pk.pack(input->GetScancodeName(sc).CString());
        eventF((void*)this, buffer.data(), buffer.size());
	}
}

void KeyEventHandler::HandleKeyDown(StringHash eventType, VariantMap& eventData)
{
	if (eventF != NULL) {
        if (!eventData[KeyDown::P_REPEAT].GetBool()) {
            msgpack::sbuffer buffer;
            msgpack::packer<msgpack::sbuffer> pk(&buffer);
            pk.pack_array(4);
            pk.pack(2);
            int sc = eventData[KeyUp::P_SCANCODE].GetInt();
            pk.pack_fix_int32(eventData[KeyUp::P_KEY].GetInt());
            pk.pack_fix_int32(sc);
            pk.pack(input->GetScancodeName(sc).CString());
            eventF((void*)this, buffer.data(), buffer.size());
        }
	}
}

void KeyEventHandler::registerEvent(msgFP f)
{
  eventF = f;
}

