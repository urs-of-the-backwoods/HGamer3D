//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/EditTextCbor.hpp

#ifndef __EditText_cbor__
#define __EditText_cbor__

#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string EditText;

void readEditText(CborValue *it, EditText *editText);
void writeEditText(CborEncoder *enc, EditText editText);

} // end of namespacd cdb

extern const uint64_t ctEditText;
#endif
