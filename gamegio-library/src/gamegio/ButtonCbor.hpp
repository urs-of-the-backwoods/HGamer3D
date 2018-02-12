//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ButtonCbor.hpp

#ifndef __Button_cbor__
#define __Button_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    bool pressed;
    std::string label;
} Button;

void readButton(CborValue *it0, Button *button);
void writeButton(CborEncoder *enc0, Button button);

} // end of namespacd cdb

extern const uint64_t ctButton;
#endif
