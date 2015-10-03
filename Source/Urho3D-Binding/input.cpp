//	C++ part of bindings for input
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/input.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "input.hpp"
#include "Urho3D/Input/InputEvents.h"
#include "Urho3D/UI/UIEvents.h"


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
}

Mouse::~Mouse()
{
  delete input;
}

int Mouse::create(char* pdata, int len)
{
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
  bool flag = obj.as<bool>();
  std::cout << "in mouse-visible: " << obj << " " << flag << std::endl;
  input->SetMouseVisible(flag);       // true -> suppress event
  return 0;
}

//
// InputEventHandler
//

InputEventHandler::InputEventHandler(Graphics3DSystem* g3ds)
: Object(g3ds->context)
{
    input = new Input(g3ds->context);
    
    mouseEventF = NULL;
    keyEventF = NULL;
    
    bDefaultEvents = true;
    bMouseEvents = false;
    bKeyEvents = false;
    bMouseButtonUp = false;
    bMouseButtonDown = false;
    bMouseMove = false;
    bMouseWheel = false;
    bMouseVisibleChanged = false;
    bKeyUp = false;
    bKeyDown = false;
}

InputEventHandler::~InputEventHandler()
{
  // deregister all events by following procedure
  bDefaultEvents = true;
  bMouseEvents = false;
  bKeyEvents = false;
  registerEvents();
  
  mouseEventF = NULL;
  keyEventF = NULL;
  delete input;
}

void InputEventHandler::registerEvents()
{
  // does the registration, depending on event flags
  UnsubscribeFromEvent(E_MOUSEBUTTONUP);
  UnsubscribeFromEvent(E_MOUSEBUTTONDOWN);
  UnsubscribeFromEvent(E_MOUSEMOVE);
  UnsubscribeFromEvent(E_MOUSEWHEEL);
  UnsubscribeFromEvent(E_MOUSEVISIBLECHANGED);
  UnsubscribeFromEvent(E_KEYUP);
  UnsubscribeFromEvent(E_KEYDOWN);
  
  if (bDefaultEvents) {
      // register only events, for which we have components active
      if (bMouseEvents) {
          SubscribeToEvent(E_MOUSEBUTTONUP, HANDLER(InputEventHandler, HandleMouseButtonUp));
          SubscribeToEvent(E_MOUSEBUTTONDOWN, HANDLER(InputEventHandler, HandleMouseButtonDown));
          SubscribeToEvent(E_MOUSEMOVE, HANDLER(InputEventHandler, HandleMouseMove));
          SubscribeToEvent(E_MOUSEWHEEL, HANDLER(InputEventHandler, HandleMouseWheel));
          SubscribeToEvent(E_MOUSEVISIBLECHANGED, HANDLER(InputEventHandler, HandleMouseVisibleChanged));
      }
      if (bKeyEvents) {
          SubscribeToEvent(E_KEYUP, HANDLER(InputEventHandler, HandleKeyUp));
          SubscribeToEvent(E_KEYDOWN, HANDLER(InputEventHandler, HandleKeyDown));
      }
      
  } else
      // register only events, for which we have a registration setting in input event handler
  {
    if (bMouseButtonUp) SubscribeToEvent(E_MOUSEBUTTONUP, HANDLER(InputEventHandler, HandleMouseButtonUp));
    if (bMouseButtonDown) SubscribeToEvent(E_MOUSEBUTTONDOWN, HANDLER(InputEventHandler, HandleMouseButtonDown));
    if (bMouseMove) SubscribeToEvent(E_MOUSEMOVE, HANDLER(InputEventHandler, HandleMouseMove));
    if (bMouseWheel) SubscribeToEvent(E_MOUSEWHEEL, HANDLER(InputEventHandler, HandleMouseWheel));
    if (bMouseVisibleChanged) SubscribeToEvent(E_MOUSEVISIBLECHANGED, HANDLER(InputEventHandler, HandleMouseVisibleChanged));
    if (bKeyUp) SubscribeToEvent(E_KEYUP, HANDLER(InputEventHandler, HandleKeyUp));
    if (bKeyDown) SubscribeToEvent(E_KEYDOWN, HANDLER(InputEventHandler, HandleKeyDown));
  }
    
}

int InputEventHandler::create(char* pdata, int len)
{
  return 0; 
}

int InputEventHandler::msgInputEventHandler(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
    
    std::cout << "input event handler: " << obj << std::endl;
    if (obj.type != msgpack::type::ARRAY) return ERROR_TYPE_NOT_KNOWN;

    // default handler
    if (obj.via.array.ptr[0].as<int>() == 0)
    {
        bDefaultEvents = true;
    } else if (obj.via.array.ptr[0].as<int>() == 1)
        
    // non default handler, registered events
    {
        bDefaultEvents = false;
        bMouseButtonUp = false;
        bMouseButtonDown = false;
        bMouseMove = false;
        bMouseWheel = false;
        bMouseVisibleChanged = false;
        bKeyUp = false;
        bKeyDown = false;
        
        msgpack::object evts_o = obj.via.array.ptr[1];
        
        std::vector<int> evts;
        evts_o.convert(&evts);
        
        for(std::vector<int>::iterator it = evts.begin(); it != evts.end(); ++it) {
            if (*it == 1) bMouseButtonUp = true;
            if (*it == 2) bMouseButtonDown = true;
            if (*it == 3) bMouseMove = true;
            if (*it == 4) bMouseWheel = true;
            if (*it == 5) bMouseVisibleChanged = true;
            if (*it == 6) bKeyUp = true;
            if (*it == 7) bKeyDown = true;
        }
    }
            
    registerEvents();
    return 0;
}

void InputEventHandler::HandleMouseButtonUp(StringHash eventType, VariantMap& eventData)
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

void InputEventHandler::HandleMouseButtonDown(StringHash eventType, VariantMap& eventData)
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

void InputEventHandler::HandleMouseMove(StringHash eventType, VariantMap& eventData)
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

void InputEventHandler::HandleMouseWheel(StringHash eventType, VariantMap& eventData)
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

void InputEventHandler::HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
		msgpack::sbuffer buffer;
		msgpack::packer<msgpack::sbuffer> pk(&buffer);
		pk.pack(eventData[MouseVisibleChanged::P_VISIBLE].GetBool());
		mouseEventF((void*)this, buffer.data(), buffer.size());
	}
}

void InputEventHandler::HandleKeyUp(StringHash eventType, VariantMap& eventData)
{
	if (keyEventF != NULL) {
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
        pk.pack_array(4);
        pk.pack(1);
        int sc = eventData[KeyUp::P_SCANCODE].GetInt();
        pk.pack_fix_int32(eventData[KeyUp::P_KEY].GetInt());
        pk.pack_fix_int32(sc);
        pk.pack(input->GetScancodeName(sc).CString());
        keyEventF((void*)this, buffer.data(), buffer.size());
	}
}

void InputEventHandler::HandleKeyDown(StringHash eventType, VariantMap& eventData)
{
	if (keyEventF != NULL) {
        if (!eventData[KeyDown::P_REPEAT].GetBool()) {
            msgpack::sbuffer buffer;
            msgpack::packer<msgpack::sbuffer> pk(&buffer);
            pk.pack_array(4);
            pk.pack(2);
            int sc = eventData[KeyUp::P_SCANCODE].GetInt();
            pk.pack_fix_int32(eventData[KeyUp::P_KEY].GetInt());
            pk.pack_fix_int32(sc);
            pk.pack(input->GetScancodeName(sc).CString());
            keyEventF((void*)this, buffer.data(), buffer.size());
        }
	}
}

void InputEventHandler::registerMouseEventFunction(msgFP f)
{
    // register events: depends on default setting
    bMouseEvents = true;
    registerEvents();
    mouseEventF = f;
}

void InputEventHandler::registerKeyEventFunction(msgFP f)
{
    // register events: depends on default setting
    bKeyEvents = true;
    registerEvents();
    keyEventF = f;
}

