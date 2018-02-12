//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ExitRequestedItem.hpp

#ifndef __exitrequesteditem_hpp__
#define __exitrequesteditem_hpp__

#include "Urho3D/Urho3DAll.h"
#include "Fresco.hpp"

using namespace Urho3D;

GCO_FACTORY_DEC(ExitRequestedItem)

class ExitRequestedItem : public Object {

URHO3D_OBJECT(ExitRequestedItem, Object);

private:
  FrMessageFn2 exitREventF;
  void* exitRDataP;
  uint64_t exitREventType;

public:
  ExitRequestedItem();
  ~ExitRequestedItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();

  void registerExitRequestedEventFunction(FrMessageFn2 f, void* p2, uint64_t erET);
  void HandleExitRequestedEvent(StringHash eventType, VariantMap& eventData);
};

#endif
