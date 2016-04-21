//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/graphics3d.hpp

#ifndef __graphics3d_hpp__
#define __graphics3d_hpp__

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

#include "Urho3D/DebugNew.h"
#include "errors.hpp"

using namespace Urho3D;

class Graphics3DSystem {

private:
  Engine* engine;
  VariantMap engineParameters;


public:

  Context* context;
  SharedPtr<Scene> scene;

  Graphics3DSystem();
  ~Graphics3DSystem();

  // initialization
  int create(char* pdata, int len);
  int msgCmdGraphics3DSystem(char* pdata, int len);
};

class HasNode
{

protected:
  SharedPtr<Node> node;

public:
  HasNode(Graphics3DSystem*g3ds);
  ~HasNode();

  int msgOri(char* pdata, int len);
  int msgPos(char* pdata, int len);
  int msgScale(char* pdata, int len);
};

class CameraItem : public HasNode
{

private:
  Graphics3DSystem* g3ds;
  SharedPtr<Viewport> viewport;
  int viewportSlot;
  void setFrustum(float nc, float fc, float fov);

public:
  CameraItem(Graphics3DSystem* g);
  ~CameraItem();
  int create(char *pdata, int len);
  int msgFrustum(char* pdata, int len);
};

class LightItem : public HasNode
{
private:
  Light* light;
  
public:
  LightItem(Graphics3DSystem*g3ds);
  ~LightItem();
  int create(char* pdata, int len);
  int msgLight(char* pdata, int len);
  int msgColour(char* pdata, int len);
};

class GeometryItem : public HasNode
{
private:
  String material;

public:
  GeometryItem(Graphics3DSystem*g3ds);
  ~GeometryItem();
  int create(char* pdata, int len);
  int msgGeometry(char* pdata, int len);
  int msgMaterial(char* pdata, int len);
  int msgColour(char* pdata, int len);
  
};


#endif
