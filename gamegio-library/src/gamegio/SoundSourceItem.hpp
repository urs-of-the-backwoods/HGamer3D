//	C++ part of bindings for audio
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/SoundSourceItem.hpp

#ifndef __soundsourceitem_hpp__
#define __soundsourceitem_hpp__

#include <iostream>
#include <fstream>
#include <string>
#include <stdint.h>
#include <stdbool.h>

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

#include "Urho3D/DebugNew.h"

using namespace Urho3D;

#include "Fresco.hpp"
#include "HasNode.hpp"

#include "SoundSourceCbor.hpp"

GIO_METHOD_DEC(SoundSourceItem, SoundSource)
GIO_METHOD_DEC(SoundSourceItem, PlayCmd)
GIO_METHOD_DEC(SoundSourceItem, Pos)
GIO_METHOD_DEC(SoundSourceItem, Scale)
GIO_METHOD_DEC(SoundSourceItem, Ori)

GCO_FACTORY_DEC(SoundSourceItem)

class SoundSourceItem : public HasNode, public GioComponentObject {

private:
  SharedPtr<SoundSource> soundSource;
  SharedPtr<Sound> sound;
  ResourceCache* cache;
  cbd::SoundType soundType;

public:
  SoundSourceItem();
  ~SoundSourceItem();

  // creation / destruction
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  // messages
  void msgSoundSource(FrMsg m, FrMsgLength l);
  void msgPlayCmd(FrMsg m, FrMsgLength l);
};


#endif
