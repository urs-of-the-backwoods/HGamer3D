//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ScreenModeItem.hpp

#ifndef __screen_mode_item_hpp__
#define __screen_mode_item_hpp__

#include "Urho3D/Urho3DAll.h"
#include "Fresco.hpp"

using namespace Urho3D;

GCO_FACTORY_DEC(ScreenModeItem)

class ScreenModeItem : public Object {

URHO3D_OBJECT(ScreenModeItem, Object);

private:
  FrMessageFn2 SMEventF;
  void* SMDataP;
  uint64_t SMEventType;

public:
  ScreenModeItem();
  ~ScreenModeItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  void registerScreenModeEventFunction(FrMessageFn2 f, void* p2, uint64_t erET);
  void HandleScreenModeEvent(StringHash eventType, VariantMap& eventData);
};

#endif
