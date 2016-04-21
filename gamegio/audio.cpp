//	C++ part of bindings for audio
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/audio.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "audio.hpp"
#include "Urho3D/Input/InputEvents.h"
#include "Urho3D/UI/UIEvents.h"


using namespace std;

extern "C" {
#include "interface.h"
}


//
// SoundSource
//

SoundSourceItem::SoundSourceItem(Graphics3DSystem* g3ds) : HasNode(g3ds)
{
    soundSource = NULL;
    sound = NULL;
    soundType = -1;
    cache = g3ds->context->GetSubsystem<ResourceCache>();
}

SoundSourceItem::~SoundSourceItem()
{
    if (soundType != -1) node->RemoveComponent<SoundSource>();
    delete sound;
}

int SoundSourceItem::create(char* pdata, int len)
{
    return 0;
}

int SoundSourceItem::msgSoundSource(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "soundsource: " << obj << std::endl;

    if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 5) return ERROR_TYPE_NOT_KNOWN;

    int flag = obj.via.array.ptr[0].as<int>();
    String res = String(obj.via.array.ptr[1].as<std::string>().c_str());
    float vol = obj.via.array.ptr[2].as<float>();
    bool loop = obj.via.array.ptr[3].as<bool>();
    String group = String(obj.via.array.ptr[4].as<std::string>().c_str());

    // create or modify sound source
    if (soundType != flag)
    {
        if (soundType != -1) node->RemoveComponent<SoundSource>();
        if (flag == 1)
            soundSource = node->CreateComponent<SoundSource3D>();
        else
            soundSource = node->CreateComponent<SoundSource>();
        soundType = flag;
    }
    // set sound source paramters
    soundSource->SetSoundType(group);
    soundSource->SetGain(vol);
    // create or modify sound itself
    if (sound != NULL && sound->GetName() != res) 
    {
        std::cout << "soundsource: " << sound->GetName().CString() << " " << res.CString() << std::endl;
        delete sound;
        sound = NULL;
    }
    // load resource
    if (sound == NULL)
    {
        sound = cache->GetResource<Sound>(res);
    }
    // modify sound
    if (sound != NULL)
    {
        sound->SetLooped(loop);
    }
    return 0;
}

int SoundSourceItem::msgPlayCmd(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "soundsource play: " << obj << std::endl;

//    if (obj.type != msgpack::type::INT) return ERROR_TYPE_NOT_KNOWN;
    
    if (sound != NULL)
    {
        // Play
        if (obj.as<int>() == 1) soundSource->Play(sound);
        // Pause (not correctly implemented, currently)
        if (obj.as<int>() == 2) soundSource->Stop();
        // Stop
        if (obj.as<int>() == 3) soundSource->Stop();
    }
    return 0;
}


//
// SoundListener
//

SoundListenerItem::SoundListenerItem(Graphics3DSystem* g3ds) : HasNode(g3ds)
{
}

SoundListenerItem::~SoundListenerItem()
{
}

int SoundListenerItem::create(char* pdata, int len)
{
    return 0;
}

int SoundListenerItem::msgSoundListener(char* pdata, int len)
{
    return 0;
}


//
// Volume
//

VolumeItem::VolumeItem(Graphics3DSystem* g3ds) : HasNode(g3ds)
{   
    audio = g3ds->context->GetSubsystem<Audio>();
}

VolumeItem::~VolumeItem()
{
}

int VolumeItem::create(char* pdata, int len)
{
    return 0;
}

int VolumeItem::msgVolume(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "volume: " << obj << std::endl;

    if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 2) return ERROR_TYPE_NOT_KNOWN;
    
    String group = String(obj.via.array.ptr[0].as<std::string>().c_str());
    float vol = obj.via.array.ptr[1].as<float>();
    
    audio->SetMasterGain(group, vol);
    return 0;
}


