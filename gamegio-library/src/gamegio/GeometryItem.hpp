//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/GeometryItem.hpp

#ifndef __geometry_item_hpp__
#define __geometry_item_hpp__

#include "Fresco.hpp"
#include "HasNode.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(GeometryItem, Geometry)
GIO_METHOD_DEC(GeometryItem, Material)
GIO_METHOD_DEC(GeometryItem, Colour)
GCO_FACTORY_DEC(GeometryItem)

class GeometryItem : public HasNode
{
private:
  String material;

public:
  GeometryItem();
  ~GeometryItem();
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void msgGeometry(FrMsg m, FrMsgLength l);
  void msgMaterial(FrMsg m, FrMsgLength l);
  void msgColour(FrMsg m, FrMsgLength l);
};


#endif
