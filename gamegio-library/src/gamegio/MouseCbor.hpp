#ifndef __Mouse_cbor__
#define __Mouse_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
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
    MButtonUpEvent = 1,
    MButtonDownEvent = 2,
    MMoveEvent = 3,
    MWheelEvent = 4,
} EnumMouseEvent;

typedef struct {
    EnumMouseEvent selector;
    struct {
        struct {
        } NoMouseEvent;
        struct {
            MouseButtonData value0;
        } MButtonUpEvent;
        struct {
            MouseButtonData value0;
        } MButtonDownEvent;
        struct {
            MouseMoveData value0;
        } MMoveEvent;
        struct {
            MouseWheelData value0;
        } MWheelEvent;
    } data;
} MouseEvent;

typedef enum {
    NoMouseClick = 0,
    MouseDownClick = 1,
    MouseUpClick = 2,
} EnumMouseClickEvent;

typedef struct {
    EnumMouseClickEvent selector;
    struct {
        struct {
        } NoMouseClick;
        struct {
            MouseButtonData value0;
        } MouseDownClick;
        struct {
            MouseButtonData value0;
        } MouseUpClick;
    } data;
} MouseClickEvent;

typedef enum {
    NoMouseMove = 0,
    MouseMove = 1,
} EnumMouseMoveEvent;

typedef struct {
    EnumMouseMoveEvent selector;
    struct {
        struct {
        } NoMouseMove;
        struct {
            MouseMoveData value0;
        } MouseMove;
    } data;
} MouseMoveEvent;

typedef enum {
    NoMouseWheel = 0,
    MouseWheel = 1,
} EnumMouseWheelEvent;

typedef struct {
    EnumMouseWheelEvent selector;
    struct {
        struct {
        } NoMouseWheel;
        struct {
            MouseWheelData value0;
        } MouseWheel;
    } data;
} MouseWheelEvent;

void readMouseMode(CborValue *it, MouseMode *mouseMode);
void writeMouseMode(CborEncoder *enc, MouseMode mouseMode);
void readMouseConfig(CborValue *it, MouseConfig *mouseConfig);
void writeMouseConfig(CborEncoder *enc, MouseConfig mouseConfig);
void readMouseButtonData(CborValue *it, MouseButtonData *mouseButtonData);
void writeMouseButtonData(CborEncoder *enc, MouseButtonData mouseButtonData);
void readMouseMoveData(CborValue *it, MouseMoveData *mouseMoveData);
void writeMouseMoveData(CborEncoder *enc, MouseMoveData mouseMoveData);
void readMouseWheelData(CborValue *it, MouseWheelData *mouseWheelData);
void writeMouseWheelData(CborEncoder *enc, MouseWheelData mouseWheelData);
void readMouseEvent(CborValue *it, MouseEvent *mouseEvent);
void writeMouseEvent(CborEncoder *enc, MouseEvent mouseEvent);
void readMouseClickEvent(CborValue *it, MouseClickEvent *mouseClickEvent);
void writeMouseClickEvent(CborEncoder *enc, MouseClickEvent mouseClickEvent);
void readMouseMoveEvent(CborValue *it, MouseMoveEvent *mouseMoveEvent);
void writeMouseMoveEvent(CborEncoder *enc, MouseMoveEvent mouseMoveEvent);
void readMouseWheelEvent(CborValue *it, MouseWheelEvent *mouseWheelEvent);
void writeMouseWheelEvent(CborEncoder *enc, MouseWheelEvent mouseWheelEvent);

} // end of namespacd cdb

extern const uint64_t ctMouseConfig;
extern const uint64_t ctMouseEvent;
extern const uint64_t ctMouseClickEvent;
extern const uint64_t ctMouseMoveEvent;
extern const uint64_t ctMouseWheelEvent;
#endif
