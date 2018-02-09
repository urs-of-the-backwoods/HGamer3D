//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/AngelCbor.hpp

#ifndef __Angle_cbor__
#define __Angle_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Rad = 0,
    Deg = 1,
} EnumAngle;

typedef struct {
    EnumAngle selector;
    struct {
        struct {
            float value0;
        } Rad;
        struct {
            float value0;
        } Deg;
    } data;
} Angle;

void readAngle(CborValue *it0, Angle *angle);
void writeAngle(CborEncoder *enc0, Angle angle);
float getAngleAsRadians(cbd::Angle a);


} // end of namespacd cdb

#endif
