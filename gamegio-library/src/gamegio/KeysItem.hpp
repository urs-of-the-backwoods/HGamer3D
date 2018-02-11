//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/KeysItem.hpp

#ifndef __keys_item_hpp__
#define __keys_item_hpp__

#include "Urho3D/Urho3DAll.h"
#include "Fresco.hpp"

using namespace Urho3D;

GCO_FACTORY_DEC(KeysItem)

class KeysItem : public Object {

URHO3D_OBJECT(KeysItem, Object);

private:
  Input* input;

  FrMessageFn2 keyEventF;
  void* keyDataP;
  uint64_t keyEventType;

public:
  KeysItem();
  ~KeysItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  void registerKeyEventFunction(FrMessageFn2 f, void* p2, uint64_t keyET);
  void HandleKeyUp(StringHash eventType, VariantMap& eventData);
  void HandleKeyDown(StringHash eventType, VariantMap& eventData);
};

#endif
