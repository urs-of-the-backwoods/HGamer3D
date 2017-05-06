#include "MouseCbor.hpp"

namespace cbd {

void readMouseMode(CborValue *it0, MouseMode *mouseMode)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseMode->selector = (EnumMouseMode)i;
    if (mouseMode->selector == 0) {
    };
    if (mouseMode->selector == 1) {
    };
    if (mouseMode->selector == 2) {
    };
    cbor_value_leave_container(it0, it);
}

void writeMouseMode(CborEncoder *enc0, MouseMode mouseMode)
{
    if (mouseMode.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMode.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (mouseMode.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMode.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (mouseMode.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMode.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readMouseConfig(CborValue *it0, MouseConfig *mouseConfig)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    readMouseMode(it, &(mouseConfig->mode));
    cbor_value_leave_container(it0, it);
}

void writeMouseConfig(CborEncoder *enc0, MouseConfig mouseConfig)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
    writeMouseMode(enc, mouseConfig.mode);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readMouseButtonData(CborValue *it0, MouseButtonData *mouseButtonData)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { int i; cbor_value_get_int(it, &i); mouseButtonData->button = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseButtonData->buttons = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseButtonData->qualifiers = (int32_t)i;} cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeMouseButtonData(CborEncoder *enc0, MouseButtonData mouseButtonData)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    cbor_encode_int(enc, (int64_t)mouseButtonData.button);
    cbor_encode_int(enc, (int64_t)mouseButtonData.buttons);
    cbor_encode_int(enc, (int64_t)mouseButtonData.qualifiers);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readMouseMoveData(CborValue *it0, MouseMoveData *mouseMoveData)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->x = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->y = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->dx = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->dy = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->buttons = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->qualifiers = (int32_t)i;} cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeMouseMoveData(CborEncoder *enc0, MouseMoveData mouseMoveData)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 6);
    cbor_encode_int(enc, (int64_t)mouseMoveData.x);
    cbor_encode_int(enc, (int64_t)mouseMoveData.y);
    cbor_encode_int(enc, (int64_t)mouseMoveData.dx);
    cbor_encode_int(enc, (int64_t)mouseMoveData.dy);
    cbor_encode_int(enc, (int64_t)mouseMoveData.buttons);
    cbor_encode_int(enc, (int64_t)mouseMoveData.qualifiers);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readMouseWheelData(CborValue *it0, MouseWheelData *mouseWheelData)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { int i; cbor_value_get_int(it, &i); mouseWheelData->wheel = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseWheelData->buttons = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseWheelData->qualifiers = (int32_t)i;} cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeMouseWheelData(CborEncoder *enc0, MouseWheelData mouseWheelData)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    cbor_encode_int(enc, (int64_t)mouseWheelData.wheel);
    cbor_encode_int(enc, (int64_t)mouseWheelData.buttons);
    cbor_encode_int(enc, (int64_t)mouseWheelData.qualifiers);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readMouseEvent(CborValue *it0, MouseEvent *mouseEvent)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseEvent->selector = (EnumMouseEvent)i;
    if (mouseEvent->selector == 0) {
    };
    if (mouseEvent->selector == 1) {
        readMouseButtonData(it, &(mouseEvent->data.MouseButtonUpEvent.value0));
    };
    if (mouseEvent->selector == 2) {
        readMouseButtonData(it, &(mouseEvent->data.MouseButtonDownEvent.value0));
    };
    if (mouseEvent->selector == 3) {
        readMouseMoveData(it, &(mouseEvent->data.MouseMoveEvent.value0));
    };
    if (mouseEvent->selector == 4) {
        readMouseWheelData(it, &(mouseEvent->data.MouseWheelEvent.value0));
    };
    cbor_value_leave_container(it0, it);
}

void writeMouseEvent(CborEncoder *enc0, MouseEvent mouseEvent)
{
    if (mouseEvent.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (mouseEvent.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseButtonData(enc, mouseEvent.data.MouseButtonUpEvent.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (mouseEvent.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseButtonData(enc, mouseEvent.data.MouseButtonDownEvent.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (mouseEvent.selector == 3) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseMoveData(enc, mouseEvent.data.MouseMoveEvent.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (mouseEvent.selector == 4) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseWheelData(enc, mouseEvent.data.MouseWheelEvent.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctMouseConfig = 0xa532f43b1c1c6bc7;
const uint64_t ctMouseEvent = 0x27eaf3fd46595d08;
