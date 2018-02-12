//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: Urho3D/gamegio-library/src/gamegio/InputEventHandlerCbor.hpp

#ifndef __InputEventHandler_cbor__
#define __InputEventHandler_cbor__

#include <vector>

#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    IEMouseButtonUp = 0,
    IEMouseButtonDown = 1,
    IEMouseMove = 2,
    IEMouseButtonWheel = 3,
    IEMouseVisible = 4,
    IEKeyUp = 5,
    IEKeyDown = 6,
    IEExitRequested = 7,
} EnumInputEventType;

typedef struct {
    EnumInputEventType selector;
    struct {
        struct {
        } IEMouseButtonUp;
        struct {
        } IEMouseButtonDown;
        struct {
        } IEMouseMove;
        struct {
        } IEMouseButtonWheel;
        struct {
        } IEMouseVisible;
        struct {
        } IEKeyUp;
        struct {
        } IEKeyDown;
        struct {
        } IEExitRequested;
    } data;
} InputEventType;

typedef enum {
    DefaultEventHandler = 0,
    SpecificEventHandler = 1,
} EnumInputEventHandler;

typedef struct {
    EnumInputEventHandler selector;
    struct {
        struct {
        } DefaultEventHandler;
        struct {
            std::vector<InputEventType> value0;
        } SpecificEventHandler;
    } data;
} InputEventHandler;

void readInputEventType(CborValue *it0, InputEventType *inputEventType);
void writeInputEventType(CborEncoder *enc0, InputEventType inputEventType);
void readInputEventHandler(CborValue *it0, InputEventHandler *inputEventHandler);
void writeInputEventHandler(CborEncoder *enc0, InputEventHandler inputEventHandler);

} // end of namespacd cdb

extern const uint64_t ctInputEventHandler;
extern const uint64_t ctExitRequestedEvent090;
#endif
