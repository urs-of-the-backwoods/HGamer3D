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

typedef enum {
    NoClick = 0,
    SingleClick = 1,
    DoubleClick = 2,
    ClickEnd = 3,
} EnumUIClickEvent;

typedef struct {
    EnumUIClickEvent selector;
    struct {
        struct {
        } NoClick;
        struct {
            std::string value0;
            Position2D value1;
            MouseButtonData value2;
        } SingleClick;
        struct {
            std::string value0;
            Position2D value1;
            MouseButtonData value2;
        } DoubleClick;
        struct {
            std::string value0;
            Position2D value1;
            MouseButtonData value2;
        } ClickEnd;
    } data;
} UIClickEvent;

typedef enum {
    NoHover = 0,
    HoverBegin = 1,
    HoverEnd = 2,
} EnumUIHoverEvent;

typedef struct {
    EnumUIHoverEvent selector;
    struct {
        struct {
        } NoHover;
        struct {
            std::string value0;
            Position2D value1;
            Position2D value2;
        } HoverBegin;
        struct {
            std::string value0;
        } HoverEnd;
    } data;
} UIHoverEvent;

typedef enum {
    NoDrag = 0,
    DragBegin = 1,
    DragMove = 2,
    DragEnd = 3,
    DragCancel = 4,
} EnumUIDragEvent;

typedef struct {
    EnumUIDragEvent selector;
    struct {
        struct {
        } NoDrag;
        struct {
            std::string value0;
            Position2D value1;
            Position2D value2;
        } DragBegin;
        struct {
            std::string value0;
            Position2D value1;
            Position2D value2;
            Position2D value3;
        } DragMove;
        struct {
            std::string value0;
            Position2D value1;
            Position2D value2;
        } DragEnd;
        struct {
            std::string value0;
            Position2D value1;
            Position2D value2;
        } DragCancel;
    } data;
} UIDragEvent;

void readUIClickEvent(CborValue *it, UIClickEvent *uIClickEvent);
void writeUIClickEvent(CborEncoder *enc, UIClickEvent uIClickEvent);
void readUIHoverEvent(CborValue *it, UIHoverEvent *uIHoverEvent);
void writeUIHoverEvent(CborEncoder *enc, UIHoverEvent uIHoverEvent);
void readUIDragEvent(CborValue *it, UIDragEvent *uIDragEvent);
void writeUIDragEvent(CborEncoder *enc, UIDragEvent uIDragEvent);

} // end of namespacd cdb

extern const uint64_t ctUIClickEvent;
extern const uint64_t ctUIHoverEvent;
extern const uint64_t ctUIDragEvent;
#endif
