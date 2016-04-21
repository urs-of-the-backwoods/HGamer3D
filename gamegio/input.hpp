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

#include "msgpack.hpp"

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

#include "errors.hpp"
#include "graphics3d.hpp"
#include "Urho3D/DebugNew.h"

using namespace Urho3D;

extern "C" {
#include "interface.h"
}

class Mouse : public Object {

URHO3D_OBJECT(Mouse, Object);

private:
  Input *input;

public:
  Mouse(Graphics3DSystem* g3ds);
  ~Mouse();
 
  int create(char* pdata, int len);
  int msgMouse(char* pdata, int len);
  int msgVisible(char* pdata, int len);
};

class InputEventHandler : public Object {

URHO3D_OBJECT(InputEventHandler, Object);

private:
  msgFP2 mouseEventF;
  void* mouseDataP;
  uint64_t mouseEventType;

  msgFP2 keyEventF;
  void* keyDataP;
  uint64_t keyEventType;

  msgFP2 exitREventF;
  void* exitRDataP;
  uint64_t exitREventType;

  Input *input;
  
  bool bDefaultEvents;          // events are not specified use properties, to check which to register
  
  bool bExitRequestedEvent;
  bool bKeyEvents;
  bool bMouseEvents;
  
  bool bExitRequested;
  bool bMouseButtonUp;
  bool bMouseButtonDown;
  bool bMouseMove;
  bool bMouseWheel;
  bool bMouseVisibleChanged;
  bool bKeyUp;
  bool bKeyDown;
  
  void registerEvents();

public:
  InputEventHandler(Graphics3DSystem* g3ds);
  ~InputEventHandler();
 
  int create(char* pdata, int len);
  int msgInputEventHandler(char* pdata, int len);

  void registerMouseEventFunction(msgFP2 f, void* p2, uint64_t mouseET);
  void registerKeyEventFunction(msgFP2 f, void* p2, uint64_t keyET);
  void registerExitRequestedEventFunction(msgFP2 f, void* p2, uint64_t erET);

  // the event handling routines
  void HandleMouseMove(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonDown(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonUp(StringHash eventType, VariantMap& eventData);
  void HandleMouseWheel(StringHash eventType, VariantMap& eventData);
  void HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData);

  void HandleExitRequestedEvent(StringHash eventType, VariantMap& eventData);
  
  void HandleKeyUp(StringHash eventType, VariantMap& eventData);
  void HandleKeyDown(StringHash eventType, VariantMap& eventData);
};

#endif
