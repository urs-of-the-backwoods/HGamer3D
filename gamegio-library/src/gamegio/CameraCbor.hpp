//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/CamerCbor.hpp

#ifndef __Camera_cbor__
#define __Camera_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

#include "ScreenRectCbor.hpp"
#include "AngleCbor.hpp"

namespace cbd {

typedef enum {
    FullViewCamera = 0,
    OverlayCamera = 1,
} EnumCamera;

typedef struct {
    EnumCamera selector;
    struct {
        struct {
        } FullViewCamera;
        struct {
            ScreenRect value0;
            float value1;
        } OverlayCamera;
    } data;
} Camera;

typedef struct {
    float nearDistance;
    float farDistance;
    Angle fieldOfViewHorizontal;
} Frustum;

void readCamera(CborValue *it0, Camera *camera);
void writeCamera(CborEncoder *enc0, Camera camera);
void readFrustum(CborValue *it0, Frustum *frustum);
void writeFrustum(CborEncoder *enc0, Frustum frustum);

} // end of namespacd cdb

extern const uint64_t ctCamera;
extern const uint64_t ctFrustum;
#endif
