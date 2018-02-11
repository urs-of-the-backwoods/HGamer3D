//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/WindowEventCbor.hpp

#ifndef __window_event_cbor__
#define __window_event_cbor__

#include <stdint.h>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    int32_t Width;
    int32_t Height;
    bool Fullscreen;
    bool Borderless;
} ScreenModeEvent;

typedef struct {} ExitRequestedEvent;

void readScreenModeEvent(CborValue *it, ScreenModeEvent *screenModeEvent);
void writeScreenModeEvent(CborEncoder *enc, ScreenModeEvent screenModeEvent);
void readExitRequestedEvent(CborValue *it, ExitRequestedEvent *exitRequestedEvent);
void writeExitRequestedEvent(CborEncoder *enc, ExitRequestedEvent exitRequestedEvent);

} // end of namespacd cdb

extern const uint64_t ctScreenModeEvent;
extern const uint64_t ctExitRequestedEvent090;
extern const uint64_t ctExitRequestedEvent;

#endif
