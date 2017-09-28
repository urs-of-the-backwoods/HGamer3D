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
#include "UIEventCbor.hpp"

using namespace std;
using namespace cbd;

//
// Mouse
//

GIO_METHOD_FUNC(Mouse, MouseConfig)
GIO_METHOD_FUNC(Mouse, Visible)

GCO_FACTORY_IMP(Mouse)
  GCO_FACTORY_METHOD(Mouse, ctMouseConfig, MouseConfig)
  GCO_FACTORY_METHOD(Mouse, ctVisible, Visible)
GCO_FACTORY_IMP_END

Mouse::Mouse() : Object(Graphics3DSystem::getG3DS()->context)
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

void Mouse::msgMouseConfig(FrMsg m, FrMsgLength l)
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
// BasicEventHandler
//

GCO_FACTORY_IMP(BasicEventHandler)
GCO_FACTORY_IMP_END

BasicEventHandler::BasicEventHandler() : Object(Graphics3DSystem::getG3DS()->context)
{
    input = Graphics3DSystem::getG3DS()->context->GetSubsystem<Input>();
    
    mouseMoveF = NULL;
    mouseClickF = NULL;
    mouseWheelF = NULL;

    keyEventF = NULL;
    exitREventF = NULL;
}

BasicEventHandler::~BasicEventHandler()
{
  // deregister all events 
  UnsubscribeFromEvent(E_MOUSEBUTTONUP);
  UnsubscribeFromEvent(E_MOUSEBUTTONDOWN);
  UnsubscribeFromEvent(E_MOUSEMOVE);
  UnsubscribeFromEvent(E_MOUSEWHEEL);
  UnsubscribeFromEvent(E_MOUSEVISIBLECHANGED);
  UnsubscribeFromEvent(E_KEYUP);
  UnsubscribeFromEvent(E_KEYDOWN);
  UnsubscribeFromEvent(E_EXITREQUESTED);
  UnsubscribeFromEvent(E_UIMOUSECLICK);
  UnsubscribeFromEvent(E_UIMOUSECLICKEND);
  UnsubscribeFromEvent(E_UIMOUSEDOUBLECLICK);
}

FrItem BasicEventHandler::msgCreate(FrMsg m, FrMsgLength l)
{
    return new BasicEventHandler();
}

void BasicEventHandler::msgDestroy()
{
    delete this;
}


void BasicEventHandler::registerMouseClickEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET)
{
    mouseClickF = f;
    mouseClickD = p2;
    mouseClickET = mouseET;

    SubscribeToEvent(E_MOUSEBUTTONUP, URHO3D_HANDLER(BasicEventHandler, HandleMouseButtonUp));
    SubscribeToEvent(E_MOUSEBUTTONDOWN, URHO3D_HANDLER(BasicEventHandler, HandleMouseButtonDown));
}

void BasicEventHandler::registerMouseMoveEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET)
{
    mouseMoveF = f;
    mouseMoveD = p2;
    mouseMoveET = mouseET;

    SubscribeToEvent(E_MOUSEMOVE, URHO3D_HANDLER(BasicEventHandler, HandleMouseMove));
}

void BasicEventHandler::registerMouseWheelEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET)
{
    mouseWheelF = f;
    mouseWheelD = p2;
    mouseWheelET = mouseET;

    SubscribeToEvent(E_MOUSEWHEEL, URHO3D_HANDLER(BasicEventHandler, HandleMouseWheel));
}

void BasicEventHandler::registerMouseEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET)
{
    registerMouseClickEventFunction(f, p2, mouseET);
    registerMouseMoveEventFunction(f, p2, mouseET);
    registerMouseWheelEventFunction(f, p2, mouseET);
    SubscribeToEvent(E_MOUSEVISIBLECHANGED, URHO3D_HANDLER(BasicEventHandler, HandleMouseVisibleChanged));
}

void BasicEventHandler::registerKeyEventFunction(FrMessageFn2 f, void* p2, uint64_t keyET)
{
    // register events: depends on default setting
    keyEventF = f;
    keyDataP = p2;
    keyEventType = keyET;

    SubscribeToEvent(E_KEYUP, URHO3D_HANDLER(BasicEventHandler, HandleKeyUp));
    SubscribeToEvent(E_KEYDOWN, URHO3D_HANDLER(BasicEventHandler, HandleKeyDown));
}

void BasicEventHandler::registerExitRequestedEventFunction(FrMessageFn2 f, void* p2, uint64_t erET)
{
  exitREventF = f;
  exitRDataP = p2;
  exitREventType = erET;

  SubscribeToEvent(E_EXITREQUESTED, URHO3D_HANDLER(BasicEventHandler, HandleExitRequestedEvent));
}

// Click
void BasicEventHandler::registerUIClickEventFunction(FrMessageFn2 f, void* p2, uint64_t cbet)
{
    cbfClick = f;
    cbdClick = p2;
    cbetClick = cbet;
    SubscribeToEvent(E_UIMOUSECLICK, URHO3D_HANDLER(BasicEventHandler, HandleClick));
    SubscribeToEvent(E_UIMOUSECLICKEND, URHO3D_HANDLER(BasicEventHandler, HandleClick));
    SubscribeToEvent(E_UIMOUSEDOUBLECLICK, URHO3D_HANDLER(BasicEventHandler, HandleClick));
}

void BasicEventHandler::HandleClick(StringHash eventType, VariantMap& eventData)
{


   if (cbfClick != NULL)
    {
        uint8_t buf[64];
        CborEncoder encoder;
        cbor_encoder_init(&encoder, buf, sizeof(buf), 0);
        cbd::UIClickEvent evt;

        if (eventType == E_UIMOUSECLICK) {
            evt.selector = cbd::SingleClick;
            // value0 -> element name
            evt.data.SingleClick.value0 = "";
            UIElement* clicked = static_cast<UIElement*>(eventData[UIMouseClick::P_ELEMENT].GetPtr()); 
            if (clicked) {
                evt.data.SingleClick.value0 = clicked->GetName().CString();
            } 
            // value1 -> mouse position
            evt.data.SingleClick.value1.x = eventData[UIMouseClick::P_X].GetInt();
            evt.data.SingleClick.value1.y = eventData[UIMouseClick::P_Y].GetInt();
            // value2 -> mouse button data
            evt.data.SingleClick.value2.button = eventData[UIMouseClick::P_BUTTON].GetInt();
            evt.data.SingleClick.value2.buttons = eventData[UIMouseClick::P_BUTTONS].GetInt();
            evt.data.SingleClick.value2.qualifiers = eventData[UIMouseClick::P_QUALIFIERS].GetInt();
        }

        if (eventType == E_UIMOUSEDOUBLECLICK) {
            evt.selector = cbd::DoubleClick;
            // value0 -> element name
            evt.data.DoubleClick.value0 = "";
            UIElement* clicked = static_cast<UIElement*>(eventData[UIMouseDoubleClick::P_ELEMENT].GetPtr()); 
            if (clicked) {
                evt.data.DoubleClick.value0 = clicked->GetName().CString();
            } 
            // value1 -> mouse position
            evt.data.DoubleClick.value1.x = eventData[UIMouseDoubleClick::P_X].GetInt();
            evt.data.DoubleClick.value1.y = eventData[UIMouseDoubleClick::P_Y].GetInt();
            // value2 -> mouse button data
            evt.data.DoubleClick.value2.button = eventData[UIMouseDoubleClick::P_BUTTON].GetInt();
            evt.data.DoubleClick.value2.buttons = eventData[UIMouseDoubleClick::P_BUTTONS].GetInt();
            evt.data.DoubleClick.value2.qualifiers = eventData[UIMouseDoubleClick::P_QUALIFIERS].GetInt();
        }

        if (eventType == E_UIMOUSECLICKEND) {
            evt.selector = cbd::ClickEnd;
            // value0 -> element name
            evt.data.ClickEnd.value0 = "";
            UIElement* clicked = static_cast<UIElement*>(eventData[UIMouseClickEnd::P_ELEMENT].GetPtr()); 
            if (clicked) {
                evt.data.ClickEnd.value0 = clicked->GetName().CString();
            } 
            // value1 -> mouse position
            evt.data.ClickEnd.value1.x = eventData[UIMouseClickEnd::P_X].GetInt();
            evt.data.ClickEnd.value1.y = eventData[UIMouseClickEnd::P_Y].GetInt();
            // value2 -> mouse button data
            evt.data.ClickEnd.value2.button = eventData[UIMouseClickEnd::P_BUTTON].GetInt();
            evt.data.ClickEnd.value2.buttons = eventData[UIMouseClickEnd::P_BUTTONS].GetInt();
            evt.data.ClickEnd.value2.qualifiers = eventData[UIMouseClickEnd::P_QUALIFIERS].GetInt();
        }

        cbd::writeUIClickEvent(&encoder, evt);
        size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
        cbfClick(cbdClick, cbetClick, buf, len);
    }
}

void BasicEventHandler::HandleMouseButtonUp(StringHash eventType, VariantMap& eventData)
{
  if (mouseClickF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MButtonUpEvent;
    mevt.data.MButtonUpEvent.value0.button = eventData[MouseButtonUp::P_BUTTON].GetInt();
    mevt.data.MButtonUpEvent.value0.buttons = eventData[MouseButtonUp::P_BUTTONS].GetInt();
    mevt.data.MButtonUpEvent.value0.qualifiers = eventData[MouseButtonUp::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseClickF(mouseClickD, mouseClickET, buf, len);
  }
}

void BasicEventHandler::HandleMouseButtonDown(StringHash eventType, VariantMap& eventData)
{
  if (mouseClickF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MButtonDownEvent;
    mevt.data.MButtonUpEvent.value0.button = eventData[MouseButtonUp::P_BUTTON].GetInt();
    mevt.data.MButtonUpEvent.value0.buttons = eventData[MouseButtonUp::P_BUTTONS].GetInt();
    mevt.data.MButtonUpEvent.value0.qualifiers = eventData[MouseButtonUp::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseClickF(mouseClickD, mouseClickET, buf, len);
  }
}

void BasicEventHandler::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
  if (mouseMoveF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MMoveEvent;
    if (input->IsMouseVisible()) {
      mevt.data.MMoveEvent.value0.x = eventData[MouseMove::P_X].GetInt();
      mevt.data.MMoveEvent.value0.y = eventData[MouseMove::P_Y].GetInt();
    } else {
      mevt.data.MMoveEvent.value0.x = 0;
      mevt.data.MMoveEvent.value0.y = 0;
    }
    mevt.data.MMoveEvent.value0.dx = eventData[MouseMove::P_DX].GetInt();
    mevt.data.MMoveEvent.value0.dy = eventData[MouseMove::P_DY].GetInt();
    mevt.data.MMoveEvent.value0.buttons = eventData[MouseMove::P_BUTTONS].GetInt();
    mevt.data.MMoveEvent.value0.qualifiers = eventData[MouseMove::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseMoveF(mouseMoveD, mouseMoveET, buf, len);
  }
}

void BasicEventHandler::HandleMouseWheel(StringHash eventType, VariantMap& eventData)
{
  if (mouseWheelF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MWheelEvent;
    mevt.data.MWheelEvent.value0.wheel = eventData[MouseWheel::P_WHEEL].GetInt();
    mevt.data.MWheelEvent.value0.buttons = eventData[MouseWheel::P_BUTTONS].GetInt();
    mevt.data.MWheelEvent.value0.qualifiers = eventData[MouseWheel::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseWheelF(mouseWheelD, mouseWheelET, buf, len);
  }
}

void BasicEventHandler::HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData)
{
    // we should connect this to visible flag
}

void BasicEventHandler::HandleKeyUp(StringHash eventType, VariantMap& eventData)
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

void BasicEventHandler::HandleKeyDown(StringHash eventType, VariantMap& eventData)
{
  if (keyEventF != NULL) {
        if (!eventData[KeyDown::P_REPEAT].GetBool()) {
          uint8_t buf[64];
          CborEncoder encoder;
          cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

          KeyEvent kevt;

          kevt.selector = KeyDownEvent;
          int scancode = eventData[KeyDown::P_SCANCODE].GetInt();
          kevt.data.KeyDownEvent.value0.key = eventData[KeyDown::P_KEY].GetInt();
          kevt.data.KeyDownEvent.value0.scancode = scancode;
          kevt.data.KeyDownEvent.value0.name = std::string(input->GetScancodeName(scancode).CString());

          writeKeyEvent(&encoder, kevt);
          size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
          keyEventF(keyDataP, keyEventType, buf, len);
        }
  }
}

void BasicEventHandler::HandleExitRequestedEvent(StringHash eventType, VariantMap& eventData)
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



//
// InputEventHandler
//

GIO_METHOD_FUNC(IEHClass, InputEventHandler)

GCO_FACTORY_IMP(IEHClass)
  GCO_FACTORY_METHOD(IEHClass, ctInputEventHandler, InputEventHandler)
GCO_FACTORY_IMP_END

IEHClass::IEHClass() 
{
}

IEHClass::~IEHClass()
{
}

FrItem IEHClass::msgCreate(FrMsg m, FrMsgLength l)
{
    return new IEHClass();
}

void IEHClass::msgDestroy()
{
    delete this;
}


void IEHClass::msgInputEventHandler(FrMsg m, FrMsgLength l)
{ 
  // this is a compatibility fake for older versions
  // the handlers, which are there will get events, independent of msgInputEventHandler configuration
}

