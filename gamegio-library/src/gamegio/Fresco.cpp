//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/Fresco.cpp

#include "Fresco.hpp"

GCOFactory::GCOFactory() {}
GCOFactory::~GCOFactory() {}

FrItem GCOFactory::createItem(FrMsg m, FrMsgLength l) { return 0; }
void GCOFactory::destroyItem(FrItem item) {}
FrMessageFn GCOFactory::getMessageFn(FrComponentType pt) {return 0; }



