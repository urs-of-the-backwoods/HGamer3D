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

  OBJECT(Mouse);

private:
  msgFP mouseEventF;
  msgFP visibleEventF;
  Input *input;

public:
  Mouse(Graphics3DSystem* g3ds);
  ~Mouse();
 
  int create(char* pdata, int len);
  int msgMouse(char* pdata, int len);
  int msgVisible(char* pdata, int len);

  void registerMouseEvent(msgFP f);
  void registerVisibleEvent(msgFP f);

  void HandleMouseMove(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonDown(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonUp(StringHash eventType, VariantMap& eventData);
  void HandleMouseWheel(StringHash eventType, VariantMap& eventData);
  void HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData);
};

class KeyEventHandler : public Object {

  OBJECT(KeyEventHandler);
  Input *input;

private:
  msgFP eventF;

public:
  KeyEventHandler(Graphics3DSystem* g3ds);
  ~KeyEventHandler();
 
  int create(char* pdata, int len);

  void registerEvent(msgFP f);

  void HandleKeyUp(StringHash eventType, VariantMap& eventData);
  void HandleKeyDown(StringHash eventType, VariantMap& eventData);
};


#endif
