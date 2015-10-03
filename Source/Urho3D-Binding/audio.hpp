//	C++ part of bindings for audio
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/audio.hpp

#ifndef __audio_hpp__
#define __audio_hpp__

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

#include <Urho3D/Audio/Audio.h>
#include <Urho3D/Audio/Sound.h>
#include <Urho3D/Audio/SoundSource.h>
#include <Urho3D/Audio/SoundSource3D.h>

#include <exception>

#include "errors.hpp"
#include "graphics3d.hpp"
#include "Urho3D/DebugNew.h"

using namespace Urho3D;

extern "C" {
#include "interface.h"
}

class SoundSourceItem : public HasNode {

private:
  SoundSource* soundSource;
  Sound* sound;
  ResourceCache* cache;
  int soundType;

public:
  SoundSourceItem(Graphics3DSystem* g3ds);
  ~SoundSourceItem();

  int create(char* pdata, int len);
  int msgSoundSource(char* pdata, int len);
  int msgPlayCmd(char* pdata, int len);
};

class SoundListenerItem : public HasNode {

private:

public:
  SoundListenerItem(Graphics3DSystem* g3ds);
  ~SoundListenerItem();

  int create(char* pdata, int len);
  int msgSoundListener(char* pdata, int len);
};

class VolumeItem : public HasNode {

private:
  Audio* audio;

public:
  VolumeItem(Graphics3DSystem* g3ds);
  ~VolumeItem();

  int create(char* pdata, int len);
  int msgVolume(char* pdata, int len);
};


#endif
