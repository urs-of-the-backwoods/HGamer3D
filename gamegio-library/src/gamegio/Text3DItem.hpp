//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/Text3DItem.hpp

#ifndef __text3d_item_hpp__
#define __text3d_item_hpp__

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
#include <Urho3D/UI/Text3D.h>
#include <Urho3D/UI/UI.h>

#include <exception>

#include "Urho3D/DebugNew.h"

#include "Fresco.hpp"
#include "HasNode.hpp"

#include <stdint.h>
#include <stdbool.h>
#include <string>

using namespace Urho3D;

GIO_METHOD_DEC(Text3DItem, Label)
GIO_METHOD_DEC(Text3DItem, Text3DItem)
GCO_FACTORY_DEC(Text3DItem)

class Text3DItem : public HasNode
{
private:
  SharedPtr<Text3D> text3d;

public:
  Text3DItem();
  ~Text3DItem();
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void msgText3DItem(FrMsg m, FrMsgLength l);
  void msgLabel(FrMsg m, FrMsgLength l);
};


#endif
