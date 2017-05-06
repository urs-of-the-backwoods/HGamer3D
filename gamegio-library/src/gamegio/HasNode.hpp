//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/HasNode.hpp

#ifndef __has_node_hpp__
#define __has_node_hpp__

#include <iostream>
#include <fstream>
#include <string>

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

#include "Urho3D/DebugNew.h"

// #include "errors.hpp"
#include "Fresco.hpp"

using namespace Urho3D;

class HasNode
{

protected:
  SharedPtr<Node> node;
//  Graphics3DSystem *g3ds;

public:
  HasNode();
//  HasNode(Graphics3DSystem*g3ds);
  ~HasNode();

  virtual void msgOri(FrMsg m, FrMsgLength l);
  virtual void msgPos(FrMsg m, FrMsgLength l);
  virtual void msgScale(FrMsg m, FrMsgLength l);
  virtual void msgParent(FrMsg m, FrMsgLength l);
  virtual void msgEntityId(FrMsg m, FrMsgLength l);
};

#endif
