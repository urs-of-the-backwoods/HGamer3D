#include "MouseCbor.hpp"

namespace cbd {

void readMouseMode(CborValue *it, MouseMode *mouseMode) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseMode->selector = (EnumMouseMode)i;
    if (mouseMode->selector == 0) {
    };
    if (mouseMode->selector == 1) {
    };
    if (mouseMode->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writeMouseMode(CborEncoder *enc, MouseMode mouseMode) {
    if (mouseMode.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseMode.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseMode.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseConfig(CborValue *it, MouseConfig *mouseConfig) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    readMouseMode(it, &(mouseConfig->mode));
cbor_value_leave_container(ita, it); }
}

void writeMouseConfig(CborEncoder *enc, MouseConfig mouseConfig) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
    writeMouseMode(enc, mouseConfig.mode);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseButtonData(CborValue *it, MouseButtonData *mouseButtonData) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); mouseButtonData->button = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseButtonData->buttons = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseButtonData->qualifiers = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeMouseButtonData(CborEncoder *enc, MouseButtonData mouseButtonData) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
    cbor_encode_int(enc, (int64_t)mouseButtonData.button);
    cbor_encode_int(enc, (int64_t)mouseButtonData.buttons);
    cbor_encode_int(enc, (int64_t)mouseButtonData.qualifiers);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseMoveData(CborValue *it, MouseMoveData *mouseMoveData) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->x = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->y = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->dx = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->dy = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->buttons = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseMoveData->qualifiers = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeMouseMoveData(CborEncoder *enc, MouseMoveData mouseMoveData) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 6);
    cbor_encode_int(enc, (int64_t)mouseMoveData.x);
    cbor_encode_int(enc, (int64_t)mouseMoveData.y);
    cbor_encode_int(enc, (int64_t)mouseMoveData.dx);
    cbor_encode_int(enc, (int64_t)mouseMoveData.dy);
    cbor_encode_int(enc, (int64_t)mouseMoveData.buttons);
    cbor_encode_int(enc, (int64_t)mouseMoveData.qualifiers);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseWheelData(CborValue *it, MouseWheelData *mouseWheelData) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); mouseWheelData->wheel = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseWheelData->buttons = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); mouseWheelData->qualifiers = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeMouseWheelData(CborEncoder *enc, MouseWheelData mouseWheelData) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
    cbor_encode_int(enc, (int64_t)mouseWheelData.wheel);
    cbor_encode_int(enc, (int64_t)mouseWheelData.buttons);
    cbor_encode_int(enc, (int64_t)mouseWheelData.qualifiers);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseEvent(CborValue *it, MouseEvent *mouseEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseEvent->selector = (EnumMouseEvent)i;
    if (mouseEvent->selector == 0) {
    };
    if (mouseEvent->selector == 1) {
        readMouseButtonData(it, &(mouseEvent->data.MButtonUpEvent.value0));
    };
    if (mouseEvent->selector == 2) {
        readMouseButtonData(it, &(mouseEvent->data.MButtonDownEvent.value0));
    };
    if (mouseEvent->selector == 3) {
        readMouseMoveData(it, &(mouseEvent->data.MMoveEvent.value0));
    };
    if (mouseEvent->selector == 4) {
        readMouseWheelData(it, &(mouseEvent->data.MWheelEvent.value0));
    };
cbor_value_leave_container(ita, it); }
}

void writeMouseEvent(CborEncoder *enc, MouseEvent mouseEvent) {
    if (mouseEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseButtonData(enc, mouseEvent.data.MButtonUpEvent.value0);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseButtonData(enc, mouseEvent.data.MButtonDownEvent.value0);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseEvent.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseMoveData(enc, mouseEvent.data.MMoveEvent.value0);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseEvent.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseEvent.selector);
        writeMouseWheelData(enc, mouseEvent.data.MWheelEvent.value0);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseClickEvent(CborValue *it, MouseClickEvent *mouseClickEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseClickEvent->selector = (EnumMouseClickEvent)i;
    if (mouseClickEvent->selector == 0) {
    };
    if (mouseClickEvent->selector == 1) {
        readMouseButtonData(it, &(mouseClickEvent->data.MouseDownClick.value0));
    };
    if (mouseClickEvent->selector == 2) {
        readMouseButtonData(it, &(mouseClickEvent->data.MouseUpClick.value0));
    };
cbor_value_leave_container(ita, it); }
}

void writeMouseClickEvent(CborEncoder *enc, MouseClickEvent mouseClickEvent) {
    if (mouseClickEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseClickEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseClickEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseClickEvent.selector);
        writeMouseButtonData(enc, mouseClickEvent.data.MouseDownClick.value0);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseClickEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseClickEvent.selector);
        writeMouseButtonData(enc, mouseClickEvent.data.MouseUpClick.value0);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseMoveEvent(CborValue *it, MouseMoveEvent *mouseMoveEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseMoveEvent->selector = (EnumMouseMoveEvent)i;
    if (mouseMoveEvent->selector == 0) {
    };
    if (mouseMoveEvent->selector == 1) {
        readMouseMoveData(it, &(mouseMoveEvent->data.MouseMove.value0));
    };
cbor_value_leave_container(ita, it); }
}

void writeMouseMoveEvent(CborEncoder *enc, MouseMoveEvent mouseMoveEvent) {
    if (mouseMoveEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseMoveEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseMoveEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseMoveEvent.selector);
        writeMouseMoveData(enc, mouseMoveEvent.data.MouseMove.value0);
cbor_encoder_close_container_checked(enca, enc); }
}

void readMouseWheelEvent(CborValue *it, MouseWheelEvent *mouseWheelEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    mouseWheelEvent->selector = (EnumMouseWheelEvent)i;
    if (mouseWheelEvent->selector == 0) {
    };
    if (mouseWheelEvent->selector == 1) {
        readMouseWheelData(it, &(mouseWheelEvent->data.MouseWheel.value0));
    };
cbor_value_leave_container(ita, it); }
}

void writeMouseWheelEvent(CborEncoder *enc, MouseWheelEvent mouseWheelEvent) {
    if (mouseWheelEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)mouseWheelEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (mouseWheelEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)mouseWheelEvent.selector);
        writeMouseWheelData(enc, mouseWheelEvent.data.MouseWheel.value0);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctMouseConfig = 0xa532f43b1c1c6bc7;
const uint64_t ctMouseEvent = 0x27eaf3fd46595d08;
const uint64_t ctMouseClickEvent = 0x5bd46a46b4ae5d38;
const uint64_t ctMouseMoveEvent = 0x7a409f478c6a34f5;
const uint64_t ctMouseWheelEvent = 0xa5d6c1c359e6d8ce;
