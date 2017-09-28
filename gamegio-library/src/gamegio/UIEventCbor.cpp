#include "UIEventCbor.hpp"

namespace cbd {

void readUIEventPosition(CborValue *it, UIEventPosition *uIEventPosition) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    readPosition2D(it, &(uIEventPosition->mousePosition));
    readPosition2D(it, &(uIEventPosition->elementPosition));
    { size_t l; cbor_value_calculate_string_length(it, &l); uIEventPosition->elementName.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(uIEventPosition->elementName.c_str()), &l, NULL); cbor_value_advance(it);}
cbor_value_leave_container(ita, it); }
}

void writeUIEventPosition(CborEncoder *enc, UIEventPosition uIEventPosition) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
    writePosition2D(enc, uIEventPosition.mousePosition);
    writePosition2D(enc, uIEventPosition.elementPosition);
    cbor_encode_text_stringz(enc, uIEventPosition.elementName.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}

void readUIClick(CborValue *it, UIClick *uIClick) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    uIClick->selector = (EnumUIClick)i;
    if (uIClick->selector == 0) {
        readUIEventPosition(it, &(uIClick->data.Down.value0));
        readMouseButtonData(it, &(uIClick->data.Down.value1));
    };
    if (uIClick->selector == 1) {
        readUIEventPosition(it, &(uIClick->data.Up.value0));
        readMouseButtonData(it, &(uIClick->data.Up.value1));
    };
cbor_value_leave_container(ita, it); }
}

void writeUIClick(CborEncoder *enc, UIClick uIClick) {
    if (uIClick.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)uIClick.selector);
        writeUIEventPosition(enc, uIClick.data.Down.value0);
        writeMouseButtonData(enc, uIClick.data.Down.value1);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIClick.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)uIClick.selector);
        writeUIEventPosition(enc, uIClick.data.Up.value0);
        writeMouseButtonData(enc, uIClick.data.Up.value1);
cbor_encoder_close_container_checked(enca, enc); }
}

void readUIHover(CborValue *it, UIHover *uIHover) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    uIHover->selector = (EnumUIHover)i;
    if (uIHover->selector == 0) {
        readUIEventPosition(it, &(uIHover->data.Hover.value0));
        readMouseButtonData(it, &(uIHover->data.Hover.value1));
    };
cbor_value_leave_container(ita, it); }
}

void writeUIHover(CborEncoder *enc, UIHover uIHover) {
    if (uIHover.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)uIHover.selector);
        writeUIEventPosition(enc, uIHover.data.Hover.value0);
        writeMouseButtonData(enc, uIHover.data.Hover.value1);
cbor_encoder_close_container_checked(enca, enc); }
}

void readUIDrag(CborValue *it, UIDrag *uIDrag) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    uIDrag->selector = (EnumUIDrag)i;
    if (uIDrag->selector == 0) {
        readUIEventPosition(it, &(uIDrag->data.Begin.value0));
        readMouseButtonData(it, &(uIDrag->data.Begin.value1));
    };
    if (uIDrag->selector == 1) {
        readUIEventPosition(it, &(uIDrag->data.Move.value0));
        readMouseButtonData(it, &(uIDrag->data.Move.value1));
    };
    if (uIDrag->selector == 2) {
        readUIEventPosition(it, &(uIDrag->data.End.value0));
        readMouseButtonData(it, &(uIDrag->data.End.value1));
    };
cbor_value_leave_container(ita, it); }
}

void writeUIDrag(CborEncoder *enc, UIDrag uIDrag) {
    if (uIDrag.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)uIDrag.selector);
        writeUIEventPosition(enc, uIDrag.data.Begin.value0);
        writeMouseButtonData(enc, uIDrag.data.Begin.value1);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIDrag.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)uIDrag.selector);
        writeUIEventPosition(enc, uIDrag.data.Move.value0);
        writeMouseButtonData(enc, uIDrag.data.Move.value1);
cbor_encoder_close_container_checked(enca, enc); }
    if (uIDrag.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
        cbor_encode_uint(enc, (uint64_t)uIDrag.selector);
        writeUIEventPosition(enc, uIDrag.data.End.value0);
        writeMouseButtonData(enc, uIDrag.data.End.value1);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctUIClick = 0x7669c3c292bf265;
const uint64_t ctUIHover = 0x7e6f5eb9ae275c30;
const uint64_t ctUIDrag = 0x239d21291230fb3a;
