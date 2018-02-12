//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ParticlesItem.cpp

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
#include "ParticlesItem.hpp"
#include "ParticlesItemCbor.hpp"
#include "VisibleCbor.hpp"
#include "Graphics3DSystem.hpp"

#include <Urho3D/Math/Vector3.h>
#include <Urho3D/Math/Quaternion.h>
#include <Urho3D/Graphics/ParticleEffect.h>

using namespace std;

GIO_METHOD_FUNC(ParticlesItem, ParticlesItem)
GIO_METHOD_FUNC(ParticlesItem, Pos)
GIO_METHOD_FUNC(ParticlesItem, Scale)
GIO_METHOD_FUNC(ParticlesItem, Ori)
GIO_METHOD_FUNC(ParticlesItem, EntityId)
GIO_METHOD_FUNC(ParticlesItem, Parent)
GIO_METHOD_FUNC(ParticlesItem, Visible)

// Factory Implementation
GCO_FACTORY_IMP(ParticlesItem)
    GCO_FACTORY_METHOD(ParticlesItem, ctParticles, ParticlesItem)
    GCO_FACTORY_METHOD(ParticlesItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(ParticlesItem, ctScale, Scale)
    GCO_FACTORY_METHOD(ParticlesItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(ParticlesItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(ParticlesItem, ctParent, Parent)
    GCO_FACTORY_METHOD(ParticlesItem, ctVisible, Visible)
GCO_FACTORY_IMP_END

ParticlesItem::ParticlesItem()
{
}

FrItem ParticlesItem::msgCreate(FrMsg m, FrMsgLength l)
{
  ParticlesItem *ti = new ParticlesItem();
  ti->emitter = ti->node->CreateComponent<ParticleEmitter>();
  return (FrItem)ti;
}

ParticlesItem::~ParticlesItem()
{
}

void ParticlesItem::msgDestroy()
{
    delete this;
}

void ParticlesItem::msgParticlesItem(FrMsg m, FrMsgLength l) {
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Particles part;
  cbd::readParticles(&it, &part);

  ResourceCache* cache = node->GetSubsystem<ResourceCache>();
  emitter->SetEffect(cache->GetResource<ParticleEffect>(part.data.ParticleEffectResource.value0.c_str()));
}

