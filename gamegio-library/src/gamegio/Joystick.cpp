//	C++ part of bindings for joystick 
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: gamegio-library/src/gamegio/Joystick.cpp

#include "Joystick.hpp"
#include "JoystickCbor.hpp"
#include "Graphics3DSystem.hpp"
#include "Urho3D/IO/Log.h"
#include <stdio.h>

using namespace Urho3D;

GCO_FACTORY_IMP(Joystick)
GCO_FACTORY_IMP_END

Joystick::Joystick()
  : Object(Graphics3DSystem::getG3DS()->context)
{
  joystickEventF = NULL;
  input =  Graphics3DSystem::getG3DS()->context->GetSubsystem<Input>();

  // register events, to receive updates on joystick connections
  SubscribeToEvent(E_JOYSTICKCONNECTED, URHO3D_HANDLER(Joystick, HandleJoystickConnected));
  SubscribeToEvent(E_JOYSTICKDISCONNECTED, URHO3D_HANDLER(Joystick, HandleJoystickDisconnected));

  // register events, to receive joystick events
  SubscribeToEvent(E_JOYSTICKBUTTONDOWN, URHO3D_HANDLER(Joystick, HandleJoystickEvents));
  SubscribeToEvent(E_JOYSTICKBUTTONUP, URHO3D_HANDLER(Joystick, HandleJoystickEvents));
  SubscribeToEvent(E_JOYSTICKAXISMOVE, URHO3D_HANDLER(Joystick, HandleJoystickEvents));
  SubscribeToEvent(E_JOYSTICKHATMOVE, URHO3D_HANDLER(Joystick, HandleJoystickEvents));
}

Joystick::~Joystick()
{
  
}

void Joystick::setIdAndState()
{
  joystickState = NULL; // reset after disconnect or connect

  int n = input->GetNumJoysticks();

  char buf[100];
  sprintf(buf, "Number of Joysticks connected: %d", n);
  Log::Write(LOG_INFO, buf);

  for ( unsigned i = 0; i < n; ++i ) {

    JoystickState *state = input->GetJoystickByIndex(i);
    if (i == joystickIndex)
      {
        char buf[100];
        sprintf(buf, "Joystick at index: %d, using Joystick %s", i, state->name_.CString());
        Log::Write(LOG_INFO, buf);
        
        joystickID = state->joystickID_;
        joystickState = state;
        break;
      }
  };
}

FrItem Joystick::msgCreate(FrMsg m, FrMsgLength l)
{
  // read message data
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Joystick jsdata;
  cbd::readJoystick(&it, &jsdata);

  // create Joystick and find the right one of index
  Joystick* joystick = new Joystick();     // get the 1st controller joystick detected
  joystick->joystickIndex = jsdata.index;  // set my index
  joystick->setIdAndState();
  return (void*)joystick;
}

void Joystick::msgDestroy()
{
  UnsubscribeFromEvent(E_JOYSTICKBUTTONDOWN);
  UnsubscribeFromEvent(E_JOYSTICKBUTTONUP);
  UnsubscribeFromEvent(E_JOYSTICKAXISMOVE);
  UnsubscribeFromEvent(E_JOYSTICKHATMOVE);
  UnsubscribeFromEvent(E_JOYSTICKCONNECTED);
  UnsubscribeFromEvent(E_JOYSTICKDISCONNECTED);
  delete this;
}

void Joystick::registerJoystickEventFunction(FrMessageFn2 f, void* p2, uint64_t joystickET)
{
  joystickDataP = p2;
  joystickEventType = joystickET;
  joystickEventF = f;
}

void Joystick::HandleJoystickConnected(StringHash eventType, VariantMap& eventData) {
  setIdAndState();
  sendJoystickChangeEvent();
}

void Joystick::HandleJoystickDisconnected(StringHash eventType, VariantMap& eventData) {
  setIdAndState();
  sendJoystickChangeEvent();
}

void Joystick::sendJoystickChangeEvent()
{
  if (joystickEventF != NULL) {
    int j = input->GetNumJoysticks();
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::JoystickEvent jevt;
    jevt.selector = cbd::JoystickChange;
    jevt.data.JoystickChange.value0 = 0;
    jevt.data.JoystickChange.value1 = 1;

    writeJoystickEvent(&encoder, jevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    joystickEventF(joystickDataP, joystickEventType, buf, len);
  }
}

void Joystick::HandleJoystickEvents(StringHash eventType, VariantMap& eventData) {
  if (joystickEventF != NULL && eventData[JoystickButtonUp::P_JOYSTICKID].GetInt() == joystickID) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::JoystickEvent jevt;

    if (eventType == E_JOYSTICKBUTTONUP) {
      jevt.selector = cbd::ButtonUp;
      jevt.data.ButtonUp.value0 = eventData[Urho3D::JoystickButtonUp::P_BUTTON].GetInt();
    }
    if (eventType == E_JOYSTICKBUTTONDOWN) {
      jevt.selector = cbd::ButtonDown;
      jevt.data.ButtonDown.value0 = eventData[Urho3D::JoystickButtonDown::P_BUTTON].GetInt();
    }
    if (eventType == E_JOYSTICKAXISMOVE) {
      jevt.selector = cbd::AxisMove;
      jevt.data.AxisMove.value0 = eventData[Urho3D::JoystickAxisMove::P_AXIS].GetInt();
      jevt.data.AxisMove.value1 = eventData[Urho3D::JoystickAxisMove::P_POSITION].GetFloat();
    }
    if (eventType == E_JOYSTICKHATMOVE) {
      jevt.selector = cbd::HatMove;
      jevt.data.HatMove.value0 = eventData[Urho3D::JoystickHatMove::P_HAT].GetInt();
      jevt.data.HatMove.value1 = eventData[Urho3D::JoystickHatMove::P_POSITION].GetInt();
    }

    writeJoystickEvent(&encoder, jevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    joystickEventF(joystickDataP, joystickEventType, buf, len);
  }
}

