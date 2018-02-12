//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/CheckBoxCbor.hpp

#ifndef __CheckBox_cbor__
#define __CheckBox_cbor__

#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef bool CheckBox;

void readCheckBox(CborValue *it, CheckBox *checkBox);
void writeCheckBox(CborEncoder *enc, CheckBox checkBox);

} // end of namespacd cdb

extern const uint64_t ctCheckBox;
#endif
