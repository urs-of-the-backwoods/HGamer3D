//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/gui.hpp

#ifndef __lineedit2_hpp__
#define __lineedit2_hpp__

#include <iostream>
#include <fstream>
#include <string>

#include "Urho3D/Urho3D.h"

#include <Urho3D/UI/LineEdit.h>

#include <exception>

#include "Urho3D/DebugNew.h"

using namespace Urho3D;

namespace Urho3D
{

class LineEdit2 : public LineEdit {
public:
  LineEdit2(Context* context);
  void SetText(const String& text);
  void UpdateText();
};

}
#endif