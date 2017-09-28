#ifndef __InputEventHandler_cbor__
#define __InputEventHandler_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
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

typedef struct {} ExitRequestedEvent;

typedef struct {} InputEvents;

void readInputEventType(CborValue *it, InputEventType *inputEventType);
void writeInputEventType(CborEncoder *enc, InputEventType inputEventType);
void readInputEventHandler(CborValue *it, InputEventHandler *inputEventHandler);
void writeInputEventHandler(CborEncoder *enc, InputEventHandler inputEventHandler);
void readExitRequestedEvent(CborValue *it, ExitRequestedEvent *exitRequestedEvent);
void writeExitRequestedEvent(CborEncoder *enc, ExitRequestedEvent exitRequestedEvent);
void readInputEvents(CborValue *it, InputEvents *inputEvents);
void writeInputEvents(CborEncoder *enc, InputEvents inputEvents);

} // end of namespacd cdb

extern const uint64_t ctInputEventHandler;
extern const uint64_t ctExitRequestedEvent;
extern const uint64_t ctInputEvents;
#endif
