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

#include "InputEventHandler.hpp"
#include "Urho3D/Input/InputEvents.h"
#include "Urho3D/UI/UIEvents.h"
#include "Fresco.hpp"
#include "VisibleCbor.hpp"
#include "Graphics3DSystem.hpp"

#include "MouseCbor.hpp"
#include "KeyEventCbor.hpp"
#include "InputEventHandlerCbor.hpp"

using namespace std;
using namespace cbd;

//
// Mouse
//

GIO_METHOD_FUNC(Mouse, Mouse)
GIO_METHOD_FUNC(Mouse, Visible)

GCO_FACTORY_IMP(Mouse)
  GCO_FACTORY_METHOD(Mouse, ctMouseConfig, Mouse)
  GCO_FACTORY_METHOD(Mouse, ctVisible, Visible)
GCO_FACTORY_IMP_END

Mouse::Mouse()
: Object(Graphics3DSystem::getG3DS()->context)
{
    input =  Graphics3DSystem::getG3DS()->context->GetSubsystem<Input>();
}

Mouse::~Mouse()
{
}

FrItem Mouse::msgCreate(FrMsg m, FrMsgLength l)
{
  return new Mouse();
}

void Mouse::msgDestroy()
{
    delete this;
}

void Mouse::msgMouse(FrMsg m, FrMsgLength l)
{
  // read message data
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::MouseConfig mConf;
  readMouseConfig(&it, &mConf);

  // set mouse mode
  if (mConf.mode.selector == cbd::Absolute)
  {
      input->SetMouseMode(MM_ABSOLUTE);
  }
  else if (mConf.mode.selector ==  cbd::Relative)
  {
      input->SetMouseMode(MM_RELATIVE);
  }
  else if (mConf.mode.selector == cbd::Wrap)
  {
      input->SetMouseMode(MM_WRAP);
  }
}
  
void Mouse::msgVisible(FrMsg m, FrMsgLength l)
{
  // read message data
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  Visible visible;
  readVisible(&it, &visible);

  input->SetMouseVisible(visible);       // true -> suppress event
}

//
// InputEventHandler
//

GIO_METHOD_FUNC(IEHClass, InputEventHandler)

GCO_FACTORY_IMP(IEHClass)
  GCO_FACTORY_METHOD(IEHClass, ctInputEventHandler, InputEventHandler)
GCO_FACTORY_IMP_END

IEHClass::IEHClass()
: Object(Graphics3DSystem::getG3DS()->context)
{
    input = Graphics3DSystem::getG3DS()->context->GetSubsystem<Input>();
    
    mouseEventF = NULL;
    mouseDataP = NULL;
    keyEventF = NULL;
    keyDataP = NULL;
    exitREventF = NULL;
    exitRDataP = NULL;
    
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

IEHClass::~IEHClass()
{
  // deregister all events by following procedure
  bDefaultEvents = true;
  bMouseEvents = false;
  bKeyEvents = false;
  bExitRequestedEvent = false;
  registerEvents();
  
  exitREventF = NULL;
  mouseEventF = NULL;
  keyEventF = NULL;
}

FrItem IEHClass::msgCreate(FrMsg m, FrMsgLength l)
{
    return new IEHClass();
}

void IEHClass::msgDestroy()
{
    delete this;
}



void IEHClass::registerEvents()
{
  // does the registration, depending on event flags
  UnsubscribeFromEvent(E_MOUSEBUTTONUP);
  UnsubscribeFromEvent(E_MOUSEBUTTONDOWN);
  UnsubscribeFromEvent(E_MOUSEMOVE);
  UnsubscribeFromEvent(E_MOUSEWHEEL);
  UnsubscribeFromEvent(E_MOUSEVISIBLECHANGED);
  UnsubscribeFromEvent(E_KEYUP);
  UnsubscribeFromEvent(E_KEYDOWN);
  UnsubscribeFromEvent(E_EXITREQUESTED);
  
  if (bDefaultEvents) {
      // register only events, for which we have components active
      if (bMouseEvents) {
          SubscribeToEvent(E_MOUSEBUTTONUP, URHO3D_HANDLER(IEHClass, HandleMouseButtonUp));
          SubscribeToEvent(E_MOUSEBUTTONDOWN, URHO3D_HANDLER(IEHClass, HandleMouseButtonDown));
          SubscribeToEvent(E_MOUSEMOVE, URHO3D_HANDLER(IEHClass, HandleMouseMove));
          SubscribeToEvent(E_MOUSEWHEEL, URHO3D_HANDLER(IEHClass, HandleMouseWheel));
          SubscribeToEvent(E_MOUSEVISIBLECHANGED, URHO3D_HANDLER(IEHClass, HandleMouseVisibleChanged));
      }
      if (bKeyEvents) {
          SubscribeToEvent(E_KEYUP, URHO3D_HANDLER(IEHClass, HandleKeyUp));
          SubscribeToEvent(E_KEYDOWN, URHO3D_HANDLER(IEHClass, HandleKeyDown));
      }
      if (bExitRequestedEvent) {
          SubscribeToEvent(E_EXITREQUESTED, URHO3D_HANDLER(IEHClass, HandleExitRequestedEvent));
      }
      
  } else
      // register only events, for which we have a registration setting in input event handler
  {
    if (bMouseButtonUp) SubscribeToEvent(E_MOUSEBUTTONUP, URHO3D_HANDLER(IEHClass, HandleMouseButtonUp));
    if (bMouseButtonDown) SubscribeToEvent(E_MOUSEBUTTONDOWN, URHO3D_HANDLER(IEHClass, HandleMouseButtonDown));
    if (bMouseMove) SubscribeToEvent(E_MOUSEMOVE, URHO3D_HANDLER(IEHClass, HandleMouseMove));
    if (bMouseWheel) SubscribeToEvent(E_MOUSEWHEEL, URHO3D_HANDLER(IEHClass, HandleMouseWheel));
    if (bMouseVisibleChanged) SubscribeToEvent(E_MOUSEVISIBLECHANGED, URHO3D_HANDLER(IEHClass, HandleMouseVisibleChanged));
    if (bKeyUp) SubscribeToEvent(E_KEYUP, URHO3D_HANDLER(IEHClass, HandleKeyUp));
    if (bKeyDown) SubscribeToEvent(E_KEYDOWN, URHO3D_HANDLER(IEHClass, HandleKeyDown));
  }
    
}

void IEHClass::msgInputEventHandler(FrMsg m, FrMsgLength l)
{ 
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    InputEventHandler ieh;
    readInputEventHandler(&it, &ieh);

    // default handler
    if (ieh.selector == DefaultEventHandler)
    {
        bDefaultEvents = true;
    } else if (ieh.selector == SpecificEventHandler)
        
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
        bExitRequested = false;
        
        for(std::vector<InputEventType>::iterator it = ieh.data.SpecificEventHandler.value0.begin(); it != ieh.data.SpecificEventHandler.value0.end(); ++it) {
            if (it->selector == IEMouseButtonUp) bMouseButtonUp = true;
            if (it->selector == IEMouseButtonDown) bMouseButtonDown = true;
            if (it->selector == IEMouseMove) bMouseMove = true;
            if (it->selector == IEMouseButtonWheel) bMouseWheel = true;
            if (it->selector == IEMouseVisible) bMouseVisibleChanged = true;
            if (it->selector == IEKeyUp) bKeyUp = true;
            if (it->selector == IEKeyDown) bKeyDown = true;
            if (it->selector == IEExitRequested) bExitRequested = true;
        }
    }
            
    registerEvents();
}

void IEHClass::HandleMouseButtonUp(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseButtonUpEvent;
    mevt.data.MouseButtonUpEvent.value0.button = eventData[MouseButtonUp::P_BUTTON].GetInt();
    mevt.data.MouseButtonUpEvent.value0.buttons = eventData[MouseButtonUp::P_BUTTONS].GetInt();
    mevt.data.MouseButtonUpEvent.value0.qualifiers = eventData[MouseButtonUp::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void IEHClass::HandleMouseButtonDown(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseButtonDownEvent;
    mevt.data.MouseButtonUpEvent.value0.button = eventData[MouseButtonUp::P_BUTTON].GetInt();
    mevt.data.MouseButtonUpEvent.value0.buttons = eventData[MouseButtonUp::P_BUTTONS].GetInt();
    mevt.data.MouseButtonUpEvent.value0.qualifiers = eventData[MouseButtonUp::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void IEHClass::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseButtonUpEvent;
    if (input->IsMouseVisible()) {
      mevt.data.MouseMoveEvent.value0.x = eventData[MouseMove::P_X].GetInt();
      mevt.data.MouseMoveEvent.value0.y = eventData[MouseMove::P_Y].GetInt();
    } else {
      mevt.data.MouseMoveEvent.value0.x = 0;
      mevt.data.MouseMoveEvent.value0.y = 0;
    }
    mevt.data.MouseMoveEvent.value0.dx = eventData[MouseMove::P_DX].GetInt();
    mevt.data.MouseMoveEvent.value0.dy = eventData[MouseMove::P_DY].GetInt();
    mevt.data.MouseMoveEvent.value0.buttons = eventData[MouseMove::P_BUTTONS].GetInt();
    mevt.data.MouseMoveEvent.value0.qualifiers = eventData[MouseMove::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void IEHClass::HandleMouseWheel(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseButtonDownEvent;
    mevt.data.MouseWheelEvent.value0.wheel = eventData[MouseWheel::P_WHEEL].GetInt();
    mevt.data.MouseWheelEvent.value0.buttons = eventData[MouseWheel::P_BUTTONS].GetInt();
    mevt.data.MouseWheelEvent.value0.qualifiers = eventData[MouseWheel::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void IEHClass::HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    // we should connect this to visible flag
	}
}

void IEHClass::HandleKeyUp(StringHash eventType, VariantMap& eventData)
{
	if (keyEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    KeyEvent kevt;

    kevt.selector = KeyUpEvent;
    int scancode = eventData[KeyUp::P_SCANCODE].GetInt();
    kevt.data.KeyUpEvent.value0.key = eventData[KeyUp::P_KEY].GetInt();
    kevt.data.KeyUpEvent.value0.scancode = scancode;
    kevt.data.KeyUpEvent.value0.name = std::string(input->GetScancodeName(scancode).CString());

    writeKeyEvent(&encoder, kevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    keyEventF(keyDataP, keyEventType, buf, len);
	}
}

void IEHClass::HandleKeyDown(StringHash eventType, VariantMap& eventData)
{
  if (keyEventF != NULL) {
        if (!eventData[KeyDown::P_REPEAT].GetBool()) {
          uint8_t buf[64];
          CborEncoder encoder;
          cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

          KeyEvent kevt;

          kevt.selector = KeyDownEvent;
          int scancode = eventData[KeyUp::P_SCANCODE].GetInt();
          kevt.data.KeyUpEvent.value0.key = eventData[KeyUp::P_KEY].GetInt();
          kevt.data.KeyUpEvent.value0.scancode = scancode;
          kevt.data.KeyUpEvent.value0.name = std::string(input->GetScancodeName(scancode).CString());

          writeKeyEvent(&encoder, kevt);
          size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
          keyEventF(keyDataP, keyEventType, buf, len);
        }
  }
}

void IEHClass::HandleExitRequestedEvent(StringHash eventType, VariantMap& eventData)
{
  if (exitREventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    ExitRequestedEvent er;

    writeExitRequestedEvent(&encoder, er);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    exitREventF(exitRDataP, exitREventType, buf, len);
  }
}

void IEHClass::registerMouseEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET)
{
    // register events: depends on default setting
    bMouseEvents = true;
    registerEvents();
    mouseEventF = f;
    mouseDataP = p2;
    mouseEventType = mouseET;
}

void IEHClass::registerKeyEventFunction(FrMessageFn2 f, void* p2, uint64_t keyET)
{
    // register events: depends on default setting
    bKeyEvents = true;
    registerEvents();
    keyEventF = f;
    keyDataP = p2;
    keyEventType = keyET;
}

void IEHClass::registerExitRequestedEventFunction(FrMessageFn2 f, void* p2, uint64_t erET)
{
  // register events: depends on default setting
  bExitRequestedEvent = true;
  registerEvents();
  exitREventF = f;
  exitRDataP = p2;
  exitREventType = erET;
}
