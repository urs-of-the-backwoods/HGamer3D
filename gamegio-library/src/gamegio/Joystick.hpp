//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/Joystick.hpp

#ifndef __joystick_hpp__
#define __joystick_hpp__

#include "Urho3D/Urho3DAll.h"
#include "Fresco.hpp"

using namespace Urho3D;

GCO_FACTORY_DEC(Joystick)

class Joystick : public Object {

  URHO3D_OBJECT(Joystick, Object);

private:
  Input *input;
  int joystickID;
  int joystickIndex;
  JoystickState* joystickState;

  FrMessageFn2 joystickEventF;
  void* joystickDataP;
  uint64_t joystickEventType;

public:
  Joystick();
  ~Joystick();

  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void setIdAndState();
  void sendJoystickChangeEvent();

  void registerJoystickEventFunction(FrMessageFn2 f, void* p2, uint64_t joystickET);
  void HandleJoystickConnected(StringHash eventType, VariantMap& eventData);
  void HandleJoystickDisconnected(StringHash eventType, VariantMap& eventData);
  void HandleJoystickEvents(StringHash eventType, VariantMap& eventData);

};

#endif
