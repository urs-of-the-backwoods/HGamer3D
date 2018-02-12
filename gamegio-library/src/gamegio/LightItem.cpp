//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/graphics3d.cpp

#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "Fresco.hpp"

#include "LightItem.hpp"
#include "LightCbor.hpp"
#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "ColourCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"
#include "VisibleCbor.hpp"


using namespace std;


GIO_METHOD_FUNC(LightItem, Light)
GIO_METHOD_FUNC(LightItem, Colour)
GIO_METHOD_FUNC(LightItem, Pos)
GIO_METHOD_FUNC(LightItem, Scale)
GIO_METHOD_FUNC(LightItem, Ori)
GIO_METHOD_FUNC(LightItem, EntityId)
GIO_METHOD_FUNC(LightItem, Parent)
GIO_METHOD_FUNC(LightItem, Visible)

// Factory Implementation
GCO_FACTORY_IMP(LightItem)
    GCO_FACTORY_METHOD(LightItem, ctLight, Light)
    GCO_FACTORY_METHOD(LightItem, ctColour, Colour)
    GCO_FACTORY_METHOD(LightItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(LightItem, ctScale, Scale)
    GCO_FACTORY_METHOD(LightItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(LightItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(LightItem, ctParent, Parent)
    GCO_FACTORY_METHOD(LightItem, ctVisible, Visible)
GCO_FACTORY_IMP_END

LightItem::LightItem()
{
}

FrItem LightItem::msgCreate(FrMsg m, FrMsgLength l)
{
  LightItem *lp = new LightItem();
  lp->light = lp->node->CreateComponent<Light>();
  return (FrItem)lp;
}

LightItem::~LightItem()
{
  node->RemoveComponent<Light>();
}

void LightItem::msgDestroy()
{
    delete this;
}

void LightItem::msgLight(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Light li;
  cbd::readLight(&it, &li);

  // Light type parameters
  if (li.type.selector == cbd::PointLight)
  {
    light->SetLightType(LIGHT_POINT);

  } else if (li.type.selector == cbd::DirectionalLight)
  {
    light->SetLightType(LIGHT_DIRECTIONAL);

  } else if (li.type.selector == cbd::SpotLight)
  {
    light->SetLightType(LIGHT_SPOT);
    light->SetFov(cbd::getAngleAsRadians(li.type.data.SpotLight.value0));
    light->SetAspectRatio(li.type.data.SpotLight.value1);
  } 

  // light parameters
  // brightness, range, spec intesity, per vertex
  light->SetBrightness(li.brightness);
  light->SetRange(li.range);
  light->SetSpecularIntensity(li.specularIntensity);
  light->SetPerVertex(false);
};

void LightItem::msgColour(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Colour c;
  cbd::readColour(&it, &c);

  // colour
  light->SetColor(Color(
      c.red,
      c.green,
      c.blue
      ));
};

