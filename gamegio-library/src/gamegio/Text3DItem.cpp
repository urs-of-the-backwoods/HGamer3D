//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/Text3DItem.cpp

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
#include "Text3DItem.hpp"
#include "Text3DItemCbor.hpp"
#include "LabelCbor.hpp"
#include "Graphics3DSystem.hpp"

#include <Urho3D/Math/Vector3.h>
#include <Urho3D/Math/Quaternion.h>

using namespace std;

GIO_METHOD_FUNC(Text3DItem, Text3DItem)
GIO_METHOD_FUNC(Text3DItem, Label)
GIO_METHOD_FUNC(Text3DItem, Pos)
GIO_METHOD_FUNC(Text3DItem, Scale)
GIO_METHOD_FUNC(Text3DItem, Ori)
GIO_METHOD_FUNC(Text3DItem, EntityId)
GIO_METHOD_FUNC(Text3DItem, Parent)

// Factory Implementation
GCO_FACTORY_IMP(Text3DItem)
    GCO_FACTORY_METHOD(Text3DItem, ctText3D, Text3DItem)
    GCO_FACTORY_METHOD(Text3DItem, ctLabel, Label)
    GCO_FACTORY_METHOD(Text3DItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(Text3DItem, ctScale, Scale)
    GCO_FACTORY_METHOD(Text3DItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(Text3DItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(Text3DItem, ctParent, Parent)
GCO_FACTORY_IMP_END

Text3DItem::Text3DItem()
{
}

FrItem Text3DItem::msgCreate(FrMsg m, FrMsgLength l)
{
  Text3DItem *ti = new Text3DItem();
  ti->text3d = ti->node->CreateComponent<Text3D>();
  return (FrItem)ti;
}

Text3DItem::~Text3DItem()
{
}

void Text3DItem::msgDestroy()
{
    delete this;
}

void Text3DItem::msgText3DItem(FrMsg m, FrMsgLength l) {
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Text3D text;
  cbd::readText3D(&it, &text);

  ResourceCache* cache = node->GetSubsystem<ResourceCache>();
  text3d->SetFont(cache->GetResource<Font>(text.Font.c_str()), text.FontSize);
  text3d->SetFaceCameraMode((Urho3D::FaceCameraMode)text.FCMode.selector);
  text3d->SetFixedScreenSize(text.FixedScreenSize);
}

void Text3DItem::msgLabel(FrMsg m, FrMsgLength l) {
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Label label;
  cbd::readLabel(&it, &label);
  text3d->SetText(label.c_str());
}
