//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ColourCbor.hpp

#ifndef __Colour_cbor__
#define __Colour_cbor__

#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    float red;
    float green;
    float blue;
    float alpha;
} Colour;

void readColour(CborValue *it0, Colour *colour);
void writeColour(CborEncoder *enc0, Colour colour);

} // end of namespacd cdb

extern const uint64_t ctColour;
#endif
