#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "JoystickCbor.hpp"

namespace cbd {

void readJoystick(CborValue *it, Joystick *joystick) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); joystick->index = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeJoystick(CborEncoder *enc, Joystick joystick) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
    cbor_encode_int(enc, (int64_t)joystick.index);
cbor_encoder_close_container_checked(enca, enc); }
}

void readJoystickEvent(CborValue *it, JoystickEvent *joystickEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    joystickEvent->selector = (EnumJoystickEvent)i;
    if (joystickEvent->selector == 0) {
    };
    if (joystickEvent->selector == 1) {
        { int i; cbor_value_get_int(it, &i); joystickEvent->data.ButtonDown.value0 = (int32_t)i;} cbor_value_advance_fixed(it);
    };
    if (joystickEvent->selector == 2) {
        { int i; cbor_value_get_int(it, &i); joystickEvent->data.ButtonUp.value0 = (int32_t)i;} cbor_value_advance_fixed(it);
    };
    if (joystickEvent->selector == 3) {
        { int i; cbor_value_get_int(it, &i); joystickEvent->data.AxisMove.value0 = (int32_t)i;} cbor_value_advance_fixed(it);
        cbor_value_get_float(it, &(joystickEvent->data.AxisMove.value1)); cbor_value_advance_fixed(it);
    };
    if (joystickEvent->selector == 4) {
        { int i; cbor_value_get_int(it, &i); joystickEvent->data.HatMove.value0 = (int32_t)i;} cbor_value_advance_fixed(it);
        { int i; cbor_value_get_int(it, &i); joystickEvent->data.HatMove.value1 = (int32_t)i;} cbor_value_advance_fixed(it);
    };
cbor_value_leave_container(ita, it); }
}

void writeJoystickEvent(CborEncoder *enc, JoystickEvent joystickEvent) {
    if (joystickEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)joystickEvent.selector);
        cbor_encode_int(enc, (int64_t)joystickEvent.data.ButtonDown.value0);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)joystickEvent.selector);
        cbor_encode_int(enc, (int64_t)joystickEvent.data.ButtonUp.value0);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickEvent.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)joystickEvent.selector);
        cbor_encode_int(enc, (int64_t)joystickEvent.data.AxisMove.value0);
        cbor_encode_float(enc, joystickEvent.data.AxisMove.value1);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickEvent.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)joystickEvent.selector);
        cbor_encode_int(enc, (int64_t)joystickEvent.data.HatMove.value0);
        cbor_encode_int(enc, (int64_t)joystickEvent.data.HatMove.value1);
cbor_encoder_close_container_checked(enca, enc); }
}

void readJoystickButton(CborValue *it, JoystickButton *joystickButton) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    joystickButton->selector = (EnumJoystickButton)i;
    if (joystickButton->selector == 0) {
    };
    if (joystickButton->selector == 1) {
    };
    if (joystickButton->selector == 2) {
    };
    if (joystickButton->selector == 3) {
    };
    if (joystickButton->selector == 4) {
    };
    if (joystickButton->selector == 5) {
    };
    if (joystickButton->selector == 6) {
    };
    if (joystickButton->selector == 7) {
    };
    if (joystickButton->selector == 8) {
    };
    if (joystickButton->selector == 9) {
    };
    if (joystickButton->selector == 10) {
    };
    if (joystickButton->selector == 11) {
    };
    if (joystickButton->selector == 12) {
    };
    if (joystickButton->selector == 13) {
    };
    if (joystickButton->selector == 14) {
    };
cbor_value_leave_container(ita, it); }
}

void writeJoystickButton(CborEncoder *enc, JoystickButton joystickButton) {
    if (joystickButton.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 5) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 6) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 7) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 8) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 9) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 10) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 11) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 12) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 13) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickButton.selector == 14) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickButton.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readJoystickAxis(CborValue *it, JoystickAxis *joystickAxis) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    joystickAxis->selector = (EnumJoystickAxis)i;
    if (joystickAxis->selector == 0) {
    };
    if (joystickAxis->selector == 1) {
    };
    if (joystickAxis->selector == 2) {
    };
    if (joystickAxis->selector == 3) {
    };
    if (joystickAxis->selector == 4) {
    };
    if (joystickAxis->selector == 5) {
    };
cbor_value_leave_container(ita, it); }
}

void writeJoystickAxis(CborEncoder *enc, JoystickAxis joystickAxis) {
    if (joystickAxis.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickAxis.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickAxis.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickAxis.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickAxis.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickAxis.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickAxis.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickAxis.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickAxis.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickAxis.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (joystickAxis.selector == 5) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)joystickAxis.selector);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctJoystick = 0xe5ea0d693a04ff71;
const uint64_t ctJoystickEvent = 0x1cdc5b0a65479346;
