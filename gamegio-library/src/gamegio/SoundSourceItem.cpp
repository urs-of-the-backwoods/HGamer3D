//	C++ part of bindings for audio
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/SoundSourceItem.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>
#include <stdint.h>
#include <stdbool.h>

#include "SoundSourceItem.hpp"
#include "SoundSourceCbor.hpp"
#include "PlayCmdCbor.hpp"
#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"

#include "Graphics3DSystem.hpp"

#include "Urho3D/Input/InputEvents.h"
#include "Urho3D/UI/UIEvents.h"

using namespace std;

// C Method for Messages
GIO_METHOD_FUNC(SoundSourceItem, SoundSource)
GIO_METHOD_FUNC(SoundSourceItem, PlayCmd)
GIO_METHOD_FUNC(SoundSourceItem, Pos)
GIO_METHOD_FUNC(SoundSourceItem, Scale)
GIO_METHOD_FUNC(SoundSourceItem, Ori)
GIO_METHOD_FUNC(SoundSourceItem, EntityId)
GIO_METHOD_FUNC(SoundSourceItem, Parent)

// Factory Implementation
GCO_FACTORY_IMP(SoundSourceItem)
    GCO_FACTORY_METHOD(SoundSourceItem, ctSoundSource, SoundSource)
    GCO_FACTORY_METHOD(SoundSourceItem, ctPlayCmd, PlayCmd)
    GCO_FACTORY_METHOD(SoundSourceItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(SoundSourceItem, ctScale, Scale)
    GCO_FACTORY_METHOD(SoundSourceItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(SoundSourceItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(SoundSourceItem, ctParent, Parent)
GCO_FACTORY_IMP_END


//
// SoundSource
//

SoundSourceItem::SoundSourceItem()
{
    soundSource = NULL;
    sound = NULL;
    Graphics3DSystem* g3ds = Graphics3DSystem::getG3DS();
    cache = g3ds->context->GetSubsystem<ResourceCache>();
}

SoundSourceItem::~SoundSourceItem()
{
    if (sound != NULL) 
    {
        delete sound;
        node->RemoveComponent<SoundSource>();
    }
}


// creation / destruction
FrItem SoundSourceItem::msgCreate(FrMsg m, FrMsgLength l)
{
    SoundSourceItem *src = new SoundSourceItem();
    return (FrItem)src;
}

void SoundSourceItem::msgDestroy()
{
    delete this;
}

void SoundSourceItem::msgSoundSource(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::SoundSource ssrc;
    cbd::readSoundSource(&it, &ssrc);

    // create or modify sound source
    if (soundType.selector != ssrc.type.selector)
    {
        if (sound != NULL) node->RemoveComponent<SoundSource>();
        if (ssrc.type.selector == cbd::Sound3D)
            soundSource = node->CreateComponent<SoundSource3D>();
        else
            soundSource = node->CreateComponent<SoundSource>();
        soundType.selector = ssrc.type.selector;
    }

    // set sound source paramters
    soundSource->SetSoundType(ssrc.volumeGroup.c_str());
    soundSource->SetGain(ssrc.volume);

    // create or modify sound itself
    if (sound != NULL && sound->GetName() != ssrc.resource.c_str()) 
    {
        delete sound;
        sound = NULL;
    }
    // load resource
    if (sound == NULL)
    {
        std::cout << ssrc.resource << std::endl;
        sound = cache->GetResource<Sound>(ssrc.resource.c_str());
    }
    // modify loop
    if (sound != NULL)
    {
        sound->SetLooped(ssrc.loop);
    }
}

void SoundSourceItem::msgPlayCmd(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::PlayCmd cmd;
    cbd::readPlayCmd(&it, &cmd);

    if (sound != NULL)
    {
        // Play
        if (cmd.selector == cbd::Play) { soundSource->Play(sound); }
        // Pause (not correctly implemented, currently)
        if (cmd.selector == cbd::Pause) { soundSource->Stop(); }
        // Stop
        if (cmd.selector == cbd::Stop) { soundSource->Stop(); }
    }
}

