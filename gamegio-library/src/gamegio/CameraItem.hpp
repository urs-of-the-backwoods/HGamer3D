//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/CamerItem.hpp

#ifndef __camera_item_hpp__
#define __camera_item_hpp__

#include <string>
#include <stdint.h>
#include <stdbool.h>

#include "Urho3D/Urho3D.h"

#include "Fresco.hpp"
#include "HasNode.hpp"
#include "Graphics3DSystem.hpp"


using namespace Urho3D;

GIO_METHOD_DEC(CameraItem, Frustum)
GCO_FACTORY_DEC(CameraItem)

class CameraItem : public HasNode
{
private:
  SharedPtr<Viewport> viewport;
  int viewportSlot;
  void setFrustum(float nc, float fc, float fov);

public:
  CameraItem();
  ~CameraItem();
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void msgFrustum(FrMsg m, FrMsgLength l);
};


#endif
