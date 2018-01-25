#ifndef __JOYSTICK_CBOR_HPP__
#define __JOYSTICK_CBOR_HPP__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    int32_t index;
} Joystick;

typedef enum {
    NoJoystickEvent = 0,
    ButtonDown = 1,
    ButtonUp = 2,
    AxisMove = 3,
    HatMove = 4,
} EnumJoystickEvent;

typedef struct {
    EnumJoystickEvent selector;
    struct {
        struct {
        } NoJoystickEvent;
        struct {
            int32_t value0;
        } ButtonDown;
        struct {
            int32_t value0;
        } ButtonUp;
        struct {
            int32_t value0;
            float value1;
        } AxisMove;
        struct {
            int32_t value0;
            int32_t value1;
        } HatMove;
    } data;
} JoystickEvent;

typedef enum {
    A = 0,
    B = 1,
    Back = 2,
    DPadDown = 3,
    DPadLeft = 4,
    DPadRight = 5,
    DPadUp = 6,
    Guide = 7,
    LeftShoulder = 8,
    LeftStick = 9,
    RightShoulder = 10,
    RightStick = 11,
    Start = 12,
    X = 13,
    Y = 14,
} EnumJoystickButton;

typedef struct {
    EnumJoystickButton selector;
    struct {
        struct {
        } A;
        struct {
        } B;
        struct {
        } Back;
        struct {
        } DPadDown;
        struct {
        } DPadLeft;
        struct {
        } DPadRight;
        struct {
        } DPadUp;
        struct {
        } Guide;
        struct {
        } LeftShoulder;
        struct {
        } LeftStick;
        struct {
        } RightShoulder;
        struct {
        } RightStick;
        struct {
        } Start;
        struct {
        } X;
        struct {
        } Y;
    } data;
} JoystickButton;

typedef enum {
    LeftX = 0,
    LeftY = 1,
    RightX = 2,
    RightY = 3,
    TriggerLeft = 4,
    TriggerRight = 5,
} EnumJoystickAxis;

typedef struct {
    EnumJoystickAxis selector;
    struct {
        struct {
        } LeftX;
        struct {
        } LeftY;
        struct {
        } RightX;
        struct {
        } RightY;
        struct {
        } TriggerLeft;
        struct {
        } TriggerRight;
    } data;
} JoystickAxis;

void readJoystick(CborValue *it, Joystick *joystick);
void writeJoystick(CborEncoder *enc, Joystick joystick);
void readJoystickEvent(CborValue *it, JoystickEvent *joystickEvent);
void writeJoystickEvent(CborEncoder *enc, JoystickEvent joystickEvent);
void readJoystickButton(CborValue *it, JoystickButton *joystickButton);
void writeJoystickButton(CborEncoder *enc, JoystickButton joystickButton);
void readJoystickAxis(CborValue *it, JoystickAxis *joystickAxis);
void writeJoystickAxis(CborEncoder *enc, JoystickAxis joystickAxis);

} // end of namespacd cdb

extern const uint64_t ctJoystick;
extern const uint64_t ctJoystickEvent;

#endif
