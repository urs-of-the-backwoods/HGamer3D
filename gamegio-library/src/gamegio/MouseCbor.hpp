#ifndef __Mouse_cbor__
#define __Mouse_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Absolute = 0,
    Relative = 1,
    Wrap = 2,
} EnumMouseMode;

typedef struct {
    EnumMouseMode selector;
    struct {
        struct {
        } Absolute;
        struct {
        } Relative;
        struct {
        } Wrap;
    } data;
} MouseMode;

typedef struct {
    MouseMode mode;
} MouseConfig;

typedef struct {
    int32_t button;
    int32_t buttons;
    int32_t qualifiers;
} MouseButtonData;

typedef struct {
    int32_t x;
    int32_t y;
    int32_t dx;
    int32_t dy;
    int32_t buttons;
    int32_t qualifiers;
} MouseMoveData;

typedef struct {
    int32_t wheel;
    int32_t buttons;
    int32_t qualifiers;
} MouseWheelData;

typedef enum {
    NoMouseEvent = 0,
    MouseButtonUpEvent = 1,
    MouseButtonDownEvent = 2,
    MouseMoveEvent = 3,
    MouseWheelEvent = 4,
} EnumMouseEvent;

typedef struct {
    EnumMouseEvent selector;
    struct {
        struct {
        } NoMouseEvent;
        struct {
            MouseButtonData value0;
        } MouseButtonUpEvent;
        struct {
            MouseButtonData value0;
        } MouseButtonDownEvent;
        struct {
            MouseMoveData value0;
        } MouseMoveEvent;
        struct {
            MouseWheelData value0;
        } MouseWheelEvent;
    } data;
} MouseEvent;

void readMouseMode(CborValue *it0, MouseMode *mouseMode);
void writeMouseMode(CborEncoder *enc0, MouseMode mouseMode);
void readMouseConfig(CborValue *it0, MouseConfig *mouseConfig);
void writeMouseConfig(CborEncoder *enc0, MouseConfig mouseConfig);
void readMouseButtonData(CborValue *it0, MouseButtonData *mouseButtonData);
void writeMouseButtonData(CborEncoder *enc0, MouseButtonData mouseButtonData);
void readMouseMoveData(CborValue *it0, MouseMoveData *mouseMoveData);
void writeMouseMoveData(CborEncoder *enc0, MouseMoveData mouseMoveData);
void readMouseWheelData(CborValue *it0, MouseWheelData *mouseWheelData);
void writeMouseWheelData(CborEncoder *enc0, MouseWheelData mouseWheelData);
void readMouseEvent(CborValue *it0, MouseEvent *mouseEvent);
void writeMouseEvent(CborEncoder *enc0, MouseEvent mouseEvent);

} // end of namespacd cdb

// compatibility mode for old InputEventHandler
extern const uint64_t ctMouseConfig;
extern const uint64_t ctMouseEvent090;

// new mouse idents
extern const uint64_t ctMouse;
extern const uint64_t ctMouseEvent;
#endif
