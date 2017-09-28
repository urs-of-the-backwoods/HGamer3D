#include "UIEventCbor.hpp"

namespace cbd {

void readUIClickEvent(CborValue *it, UIClickEvent *uIClickEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    uIClickEvent->selector = (EnumUIClickEvent)i;
    if (uIClickEvent->selector == 0) {
    };
    if (uIClickEvent->selector == 1) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIClickEvent->data.SingleClick.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIClickEvent->data.SingleClick.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIClickEvent->data.SingleClick.value1));
        readMouseButtonData(it, &(uIClickEvent->data.SingleClick.value2));
    };
    if (uIClickEvent->selector == 2) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIClickEvent->data.DoubleClick.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIClickEvent->data.DoubleClick.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIClickEvent->data.DoubleClick.value1));
        readMouseButtonData(it, &(uIClickEvent->data.DoubleClick.value2));
    };
    if (uIClickEvent->selector == 3) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIClickEvent->data.ClickEnd.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIClickEvent->data.ClickEnd.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIClickEvent->data.ClickEnd.value1));
        readMouseButtonData(it, &(uIClickEvent->data.ClickEnd.value2));
    };
cbor_value_leave_container(ita, it); }
}

void writeUIClickEvent(CborEncoder *enc, UIClickEvent uIClickEvent) {
    if (uIClickEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)uIClickEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIClickEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIClickEvent.selector);
        cbor_encode_text_stringz(enc, uIClickEvent.data.SingleClick.value0.c_str());
        writePosition2D(enc, uIClickEvent.data.SingleClick.value1);
        writeMouseButtonData(enc, uIClickEvent.data.SingleClick.value2);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIClickEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIClickEvent.selector);
        cbor_encode_text_stringz(enc, uIClickEvent.data.DoubleClick.value0.c_str());
        writePosition2D(enc, uIClickEvent.data.DoubleClick.value1);
        writeMouseButtonData(enc, uIClickEvent.data.DoubleClick.value2);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIClickEvent.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIClickEvent.selector);
        cbor_encode_text_stringz(enc, uIClickEvent.data.ClickEnd.value0.c_str());
        writePosition2D(enc, uIClickEvent.data.ClickEnd.value1);
        writeMouseButtonData(enc, uIClickEvent.data.ClickEnd.value2);
cbor_encoder_close_container_checked(enca, enc); }
}

void readUIHoverEvent(CborValue *it, UIHoverEvent *uIHoverEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    uIHoverEvent->selector = (EnumUIHoverEvent)i;
    if (uIHoverEvent->selector == 0) {
    };
    if (uIHoverEvent->selector == 1) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIHoverEvent->data.HoverBegin.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIHoverEvent->data.HoverBegin.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIHoverEvent->data.HoverBegin.value1));
        readPosition2D(it, &(uIHoverEvent->data.HoverBegin.value2));
    };
    if (uIHoverEvent->selector == 2) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIHoverEvent->data.HoverEnd.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIHoverEvent->data.HoverEnd.value0.c_str()), &l, NULL); cbor_value_advance(it);}
    };
cbor_value_leave_container(ita, it); }
}

void writeUIHoverEvent(CborEncoder *enc, UIHoverEvent uIHoverEvent) {
    if (uIHoverEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)uIHoverEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIHoverEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIHoverEvent.selector);
        cbor_encode_text_stringz(enc, uIHoverEvent.data.HoverBegin.value0.c_str());
        writePosition2D(enc, uIHoverEvent.data.HoverBegin.value1);
        writePosition2D(enc, uIHoverEvent.data.HoverBegin.value2);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIHoverEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)uIHoverEvent.selector);
        cbor_encode_text_stringz(enc, uIHoverEvent.data.HoverEnd.value0.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}

void readUIDragEvent(CborValue *it, UIDragEvent *uIDragEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    uIDragEvent->selector = (EnumUIDragEvent)i;
    if (uIDragEvent->selector == 0) {
    };
    if (uIDragEvent->selector == 1) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIDragEvent->data.DragBegin.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIDragEvent->data.DragBegin.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIDragEvent->data.DragBegin.value1));
        readPosition2D(it, &(uIDragEvent->data.DragBegin.value2));
    };
    if (uIDragEvent->selector == 2) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIDragEvent->data.DragMove.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIDragEvent->data.DragMove.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIDragEvent->data.DragMove.value1));
        readPosition2D(it, &(uIDragEvent->data.DragMove.value2));
        readPosition2D(it, &(uIDragEvent->data.DragMove.value3));
    };
    if (uIDragEvent->selector == 3) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIDragEvent->data.DragEnd.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIDragEvent->data.DragEnd.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIDragEvent->data.DragEnd.value1));
        readPosition2D(it, &(uIDragEvent->data.DragEnd.value2));
    };
    if (uIDragEvent->selector == 4) {
        { size_t l; cbor_value_calculate_string_length(it, &l); uIDragEvent->data.DragCancel.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIDragEvent->data.DragCancel.value0.c_str()), &l, NULL); cbor_value_advance(it);}
        readPosition2D(it, &(uIDragEvent->data.DragCancel.value1));
        readPosition2D(it, &(uIDragEvent->data.DragCancel.value2));
    };
cbor_value_leave_container(ita, it); }
}

void writeUIDragEvent(CborEncoder *enc, UIDragEvent uIDragEvent) {
    if (uIDragEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)uIDragEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIDragEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIDragEvent.selector);
        cbor_encode_text_stringz(enc, uIDragEvent.data.DragBegin.value0.c_str());
        writePosition2D(enc, uIDragEvent.data.DragBegin.value1);
        writePosition2D(enc, uIDragEvent.data.DragBegin.value2);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIDragEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 5);
        cbor_encode_uint(enc, (uint64_t)uIDragEvent.selector);
        cbor_encode_text_stringz(enc, uIDragEvent.data.DragMove.value0.c_str());
        writePosition2D(enc, uIDragEvent.data.DragMove.value1);
        writePosition2D(enc, uIDragEvent.data.DragMove.value2);
        writePosition2D(enc, uIDragEvent.data.DragMove.value3);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIDragEvent.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIDragEvent.selector);
        cbor_encode_text_stringz(enc, uIDragEvent.data.DragEnd.value0.c_str());
        writePosition2D(enc, uIDragEvent.data.DragEnd.value1);
        writePosition2D(enc, uIDragEvent.data.DragEnd.value2);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIDragEvent.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
        cbor_encode_uint(enc, (uint64_t)uIDragEvent.selector);
        cbor_encode_text_stringz(enc, uIDragEvent.data.DragCancel.value0.c_str());
        writePosition2D(enc, uIDragEvent.data.DragCancel.value1);
        writePosition2D(enc, uIDragEvent.data.DragCancel.value2);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctUIClickEvent = 0x7669c3c292bf265;
const uint64_t ctUIHoverEvent = 0x7e6f5eb9ae275c30;
const uint64_t ctUIDragEvent = 0x239d21291230fb3a;
