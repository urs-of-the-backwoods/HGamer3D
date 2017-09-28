#ifndef __UIEvent_cbor__
#define __UIEvent_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

#include "IntVec2Cbor.hpp"
#include "MouseCbor.hpp"

namespace cbd {

typedef struct {
    Position2D mousePosition;
    Position2D elementPosition;
    std::string elementName;
} UIEventPosition;

typedef enum {
    Down = 0,
    Up = 1,
} EnumUIClick;

typedef struct {
    EnumUIClick selector;
    struct {
        struct {
            UIEventPosition value0;
            MouseButtonData value1;
        } Down;
        struct {
            UIEventPosition value0;
            MouseButtonData value1;
        } Up;
    } data;
} UIClick;

typedef enum {
    Hover = 0,
} EnumUIHover;

typedef struct {
    EnumUIHover selector;
    struct {
        struct {
            UIEventPosition value0;
            MouseButtonData value1;
        } Hover;
    } data;
} UIHover;

typedef enum {
    Begin = 0,
    Move = 1,
    End = 2,
} EnumUIDrag;

typedef struct {
    EnumUIDrag selector;
    struct {
        struct {
            UIEventPosition value0;
            MouseButtonData value1;
        } Begin;
        struct {
            UIEventPosition value0;
            MouseButtonData value1;
        } Move;
        struct {
            UIEventPosition value0;
            MouseButtonData value1;
        } End;
    } data;
} UIDrag;

void readUIEventPosition(CborValue *it, UIEventPosition *uIEventPosition);
void writeUIEventPosition(CborEncoder *enc, UIEventPosition uIEventPosition);
void readUIClick(CborValue *it, UIClick *uIClick);
void writeUIClick(CborEncoder *enc, UIClick uIClick);
void readUIHover(CborValue *it, UIHover *uIHover);
void writeUIHover(CborEncoder *enc, UIHover uIHover);
void readUIDrag(CborValue *it, UIDrag *uIDrag);
void writeUIDrag(CborEncoder *enc, UIDrag uIDrag);

} // end of namespacd cdb

extern const uint64_t ctUIClick;
extern const uint64_t ctUIHover;
extern const uint64_t ctUIDrag;
#endif
