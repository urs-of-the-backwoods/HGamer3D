//	C++ part of bindings for audio
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/VolumeItem.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>
#include <stdint.h>
#include <stdbool.h>

#include "Urho3D/Input/InputEvents.h"
#include "Urho3D/UI/UIEvents.h"

#include "VolumeItem.hpp"
#include "VolumeCbor.hpp"
#include "Graphics3DSystem.hpp"

using namespace std;
using namespace cbd;

// C Method for Messages
GIO_METHOD_FUNC(VolumeItem, Volume)

// Factory Implementation
GCO_FACTORY_IMP(VolumeItem)
    GCO_FACTORY_METHOD(VolumeItem, ctVolume, Volume)
GCO_FACTORY_IMP_END

//
// Volume
//

VolumeItem::VolumeItem()
{
    Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
    audio = g3ds->context->GetSubsystem<Audio>();
}

VolumeItem::~VolumeItem()
{
}

FrItem VolumeItem::msgCreate(FrMsg m, FrMsgLength l)
{
    VolumeItem* vi = new VolumeItem();
    return (FrItem) vi;
}

void VolumeItem::msgDestroy()
{
    delete this;
}

// messages
void VolumeItem::msgVolume(FrMsg m, FrMsgLength l)
{
    // read message data
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    Volume volume;
    readVolume(&it, &volume);

    audio->SetMasterGain(volume.group.c_str(), volume.gain);
}

