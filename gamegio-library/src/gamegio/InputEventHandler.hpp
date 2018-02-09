//	C++ part of bindings for input
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/input.hpp

#ifndef __input_hpp__
#define __input_hpp__

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include <stdint.h>
#include <stdbool.h>

#include "Urho3D/Urho3D.h"

#include "Urho3D/Core/Context.h"
#include "Urho3D/Core/Main.h"
#include "Urho3D/Core/Object.h"

#include "Urho3D/Engine/Application.h"
#include "Urho3D/Engine/Engine.h"
#include "Urho3D/Graphics/Graphics.h"
#include "Urho3D/Graphics/GraphicsImpl.h"
#include "Urho3D/IO/IOEvents.h"
#include "Urho3D/IO/Log.h"
#include "Urho3D/Core/ProcessUtils.h"

#include <Urho3D/Graphics/Camera.h>
#include <Urho3D/Core/CoreEvents.h>
#include <Urho3D/UI/Font.h>
#include <Urho3D/Input/Input.h>
#include <Urho3D/Graphics/Material.h>
#include <Urho3D/Graphics/Model.h>
#include <Urho3D/Graphics/Octree.h>
#include <Urho3D/Graphics/Renderer.h>
#include <Urho3D/Resource/ResourceCache.h>
#include <Urho3D/Scene/Scene.h>
#include <Urho3D/Graphics/StaticModel.h>
#include <Urho3D/UI/Text.h>
#include <Urho3D/UI/UI.h>

#include <exception>

// #include "graphics3d.hpp"
#include "Urho3D/DebugNew.h"
#include "Fresco.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(Mouse, MouseConfig)
GIO_METHOD_DEC(Mouse, Visible)

GCO_FACTORY_DEC(Mouse)

class Mouse : public Object {

URHO3D_OBJECT(Mouse, Object);

private:
  Input* input;

public:
  Mouse();
  ~Mouse();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgMouseConfig(FrMsg m, FrMsgLength l);
  void msgVisible(FrMsg m, FrMsgLength l);
  void msgDestroy();
};

GIO_METHOD_DEC(IEHClass, InputEventHandler)
GCO_FACTORY_DEC(IEHClass)

class IEHClass : public Object {

URHO3D_OBJECT(IEHClass, Object);

private:
  FrMessageFn2 mouseEventF;
  void* mouseDataP;
  uint64_t mouseEventType;

  FrMessageFn2 keyEventF;
  void* keyDataP;
  uint64_t keyEventType;

  FrMessageFn2 exitREventF;
  void* exitRDataP;
  uint64_t exitREventType;

  FrMessageFn2 SMEventF;
  void* SMDataP;
  uint64_t SMEventType;

  Input* input;

  bool bDefaultEvents;          // events are not specified use properties, to check which to register

  bool bKeyEvents;
  bool bMouseEvents;

  bool bExitRequested;
  bool bScreenMode;
  bool bScreenModeRequested;
  bool bMouseButtonUp;
  bool bMouseButtonDown;
  bool bMouseMove;
  bool bMouseWheel;
  bool bMouseVisibleChanged;
  bool bKeyUp;
  bool bKeyDown;

  void registerEvents();

public:
  IEHClass();
  ~IEHClass();

   // creation / destruction
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void msgInputEventHandler(FrMsg m, FrMsgLength l);

  void registerMouseEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET);
  void registerKeyEventFunction(FrMessageFn2 f, void* p2, uint64_t keyET);
  void registerExitRequestedEventFunction(FrMessageFn2 f, void* p2, uint64_t erET);
  void registerScreenModeEventFunction(FrMessageFn2 f, void* p2, uint64_t erET);

  // the event handling routines
  void HandleMouseMove(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonDown(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonUp(StringHash eventType, VariantMap& eventData);
  void HandleMouseWheel(StringHash eventType, VariantMap& eventData);
  void HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData);

  void HandleExitRequestedEvent(StringHash eventType, VariantMap& eventData);
  void HandleScreenModeEvent(StringHash eventType, VariantMap& eventData);
  
  void HandleKeyUp(StringHash eventType, VariantMap& eventData);
  void HandleKeyDown(StringHash eventType, VariantMap& eventData);
};

#endif
