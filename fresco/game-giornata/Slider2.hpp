//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/gui.hpp

#ifndef __slider2_hpp__
#define __slider2_hpp__

#include <iostream>
#include <fstream>
#include <string>

#include "msgpack.hpp"

#include "Urho3D/Urho3D.h"

#include <Urho3D/UI/Slider.h>

#include <exception>

#include "errors.hpp"
#include "graphics3d.hpp"
#include "Urho3D/DebugNew.h"

using namespace Urho3D;

namespace Urho3D
{

class Slider2 : public Slider {
public:
  Slider2(Context* context);
  void SetValue(float value);
};

}
#endif