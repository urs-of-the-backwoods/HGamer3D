//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/MouseItem.hpp

#ifndef __mouse_item_hpp__
#define __mouse_item_hpp__

#include "Urho3D/Urho3DAll.h"
#include "Fresco.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(MouseItem, MouseMode)
GIO_METHOD_DEC(MouseItem, Visible)

GCO_FACTORY_DEC(MouseItem)

class MouseItem : public Object {

URHO3D_OBJECT(MouseItem, Object);

private:
  Input* input;
  FrMessageFn2 mouseEventF;
  void* mouseDataP;
  uint64_t mouseEventType;

public:
  MouseItem();
  ~MouseItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgMouseMode(FrMsg m, FrMsgLength l);
  void msgVisible(FrMsg m, FrMsgLength l);
  void msgDestroy();

  // the event handling routines
  void registerMouseEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET);
  void HandleMouseMove(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonDown(StringHash eventType, VariantMap& eventData);
  void HandleMouseButtonUp(StringHash eventType, VariantMap& eventData);
  void HandleMouseWheel(StringHash eventType, VariantMap& eventData);
  void HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData);
};

#endif
