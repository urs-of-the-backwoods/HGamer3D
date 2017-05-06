//	C++ part of bindings for audio
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/SoundListenerItem.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "Urho3D/Input/InputEvents.h"
#include "Urho3D/UI/UIEvents.h"

#include "Fresco.hpp"
#include "SoundListenerItem.hpp"
#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"

using namespace std;

GIO_METHOD_FUNC(SoundListenerItem, Pos)
GIO_METHOD_FUNC(SoundListenerItem, Scale)
GIO_METHOD_FUNC(SoundListenerItem, Ori)
GIO_METHOD_FUNC(SoundListenerItem, EntityId)
GIO_METHOD_FUNC(SoundListenerItem, Parent)

GCO_FACTORY_IMP(SoundListenerItem)
    GCO_FACTORY_METHOD(SoundListenerItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(SoundListenerItem, ctScale, Scale)
    GCO_FACTORY_METHOD(SoundListenerItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(SoundListenerItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(SoundListenerItem, ctParent, Parent)
GCO_FACTORY_IMP_END

//
// SoundListener
//

SoundListenerItem::SoundListenerItem()
{
}

SoundListenerItem::~SoundListenerItem()
{
}

FrItem SoundListenerItem::msgCreate(FrMsg m, FrMsgLength l)
{
    return new SoundListenerItem();
}

void SoundListenerItem::msgDestroy()
{
    delete this;
}



