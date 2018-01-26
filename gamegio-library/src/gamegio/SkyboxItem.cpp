//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/SkyboxItem.cpp

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
#include "SkyboxItem.hpp"
#include "SkyboxItemCbor.hpp"
#include "Graphics3DSystem.hpp"

#include <Urho3D/Math/Vector3.h>
#include <Urho3D/Math/Quaternion.h>

using namespace std;


// Factory Implementation
GCO_FACTORY_IMP(SkyboxItem)
GCO_FACTORY_IMP_END

SkyboxItem::SkyboxItem()
{
}

FrItem SkyboxItem::msgCreate(FrMsg m, FrMsgLength l)
{
  SkyboxItem *si = new SkyboxItem();

  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Skybox sb;
  cbd::readSkybox(&it, &sb);
  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
  ResourceCache* cache = si->node->GetSubsystem<ResourceCache>();
  SharedPtr<File> file;

  if (sb.selector == cbd::SkyboxMaterial) {
    Skybox* skybox=si->node->CreateComponent<Skybox>();
    skybox->SetModel(cache->GetResource<Model>("Models/Box.mdl"));
    skybox->SetMaterial(cache->GetResource<Material>(sb.data.SkyboxMaterial.value0.c_str()));
  }

  return (FrItem)si;
}

SkyboxItem::~SkyboxItem()
{
}

void SkyboxItem::msgDestroy()
{
    delete this;
}

