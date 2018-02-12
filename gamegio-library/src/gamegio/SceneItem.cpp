//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/SceneItem.cpp

#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "Fresco.hpp"

#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"
#include "SceneItem.hpp"
#include "SceneItemCbor.hpp"
#include "Graphics3DSystem.hpp"
#include "VisibleCbor.hpp"

#include <Urho3D/Math/Vector3.h>
#include <Urho3D/Math/Quaternion.h>

using namespace std;

GIO_METHOD_FUNC(SceneItem, Pos)
GIO_METHOD_FUNC(SceneItem, Scale)
GIO_METHOD_FUNC(SceneItem, Ori)
GIO_METHOD_FUNC(SceneItem, EntityId)
GIO_METHOD_FUNC(SceneItem, Parent)
GIO_METHOD_FUNC(SceneItem, Visible)

// Factory Implementation
GCO_FACTORY_IMP(SceneItem)
    GCO_FACTORY_METHOD(SceneItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(SceneItem, ctScale, Scale)
    GCO_FACTORY_METHOD(SceneItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(SceneItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(SceneItem, ctParent, Parent)
    GCO_FACTORY_METHOD(SceneItem, ctVisible, Visible)
GCO_FACTORY_IMP_END

SceneItem::SceneItem()
{
}

FrItem SceneItem::msgCreate(FrMsg m, FrMsgLength l)
{
  SceneItem *si = new SceneItem();

  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Scene scene;
  cbd::readScene(&it, &scene);
  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
  ResourceCache* cache = si->node->GetSubsystem<ResourceCache>();
  SharedPtr<File> file;

  if (scene.selector == cbd::XmlScene) {
    file = cache->GetFile(scene.data.XmlScene.value0.c_str());
    si->node = g3ds->scene->InstantiateXML(*file, Vector3::ZERO, Quaternion::IDENTITY);
  }

  if (scene.selector == cbd::BinaryScene) {
    file = cache->GetFile(scene.data.BinaryScene.value0.c_str());
    si->node = g3ds->scene->Instantiate(*file, Vector3::ZERO, Quaternion::IDENTITY);
  }

  return (FrItem)si;
}

SceneItem::~SceneItem()
{
}

void SceneItem::msgDestroy()
{
    delete this;
}

