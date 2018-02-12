//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/Graphics3DSystem.hpp

#ifndef __graphics3dsystem_hpp__
#define __graphics3dsystem_hpp__

#include <Urho3D/Urho3DAll.h>
#include "Graphics3DConfigCbor.hpp"
#include "EntityIdCbor.hpp"
#include "Fresco.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(Graphics3DSystem, Command)
GCO_FACTORY_DEC(Graphics3DSystem)

class Graphics3DSystem {

private:
  Engine* engine;
  static Graphics3DSystem* singleton;

public:
  Context* context;
  SharedPtr<Scene> scene;

  std::map<cbd::EntityId, Node* > node_map;
  std::map<cbd::EntityId, UIElement* > ui_map;

  Graphics3DSystem();
  ~Graphics3DSystem();

  static Graphics3DSystem* getG3DS() { return singleton; };

  // creation / destruction
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  // messages
  void msgCommand(FrMsg m, FrMsgLength l);
};


#endif
