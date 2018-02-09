//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/LineEdit2.hpp

#ifndef __lineedit2_hpp__
#define __lineedit2_hpp__

#include "Urho3D/Urho3DAll.h"

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
