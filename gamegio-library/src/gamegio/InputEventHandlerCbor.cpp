#include "InputEventHandlerCbor.hpp"

namespace cbd {

void readInputEventType(CborValue *it, InputEventType *inputEventType) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    inputEventType->selector = (EnumInputEventType)i;
    if (inputEventType->selector == 0) {
    };
    if (inputEventType->selector == 1) {
    };
    if (inputEventType->selector == 2) {
    };
    if (inputEventType->selector == 3) {
    };
    if (inputEventType->selector == 4) {
    };
    if (inputEventType->selector == 5) {
    };
    if (inputEventType->selector == 6) {
    };
    if (inputEventType->selector == 7) {
    };
cbor_value_leave_container(ita, it); }
}

void writeInputEventType(CborEncoder *enc, InputEventType inputEventType) {
    if (inputEventType.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 5) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 6) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventType.selector == 7) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readInputEventHandler(CborValue *it, InputEventHandler *inputEventHandler) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    inputEventHandler->selector = (EnumInputEventHandler)i;
    if (inputEventHandler->selector == 0) {
    };
    if (inputEventHandler->selector == 1) {
        { CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
  size_t l; cbor_value_get_array_length(it, &l); 
  inputEventHandler->data.SpecificEventHandler.value0.clear();
  while (!cbor_value_at_end(it)) {   InputEventType item; readInputEventType(it, &(item));
; inputEventHandler->data.SpecificEventHandler.value0.push_back(item); } 
cbor_value_leave_container(ita, it); }
    };
cbor_value_leave_container(ita, it); }
}

void writeInputEventHandler(CborEncoder *enc, InputEventHandler inputEventHandler) {
    if (inputEventHandler.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventHandler.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (inputEventHandler.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)inputEventHandler.selector);
        { size_t l; l = inputEventHandler.data.SpecificEventHandler.value0.size(); 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, l);
for (int i = 0; i < l; i++) {writeInputEventType(enc, inputEventHandler.data.SpecificEventHandler.value0[i]);
; }cbor_encoder_close_container_checked(enca, enc); }
}
cbor_encoder_close_container_checked(enca, enc); }
}

void readExitRequestedEvent(CborValue *it, ExitRequestedEvent *exitRequestedEvent) {
    { uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);
}

void writeExitRequestedEvent(CborEncoder *enc, ExitRequestedEvent exitRequestedEvent) {
    cbor_encode_simple_value(enc, NullValue);
}

void readInputEvents(CborValue *it, InputEvents *inputEvents) {
    { uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);
}

void writeInputEvents(CborEncoder *enc, InputEvents inputEvents) {
    cbor_encode_simple_value(enc, NullValue);
}


} // end of namespacd cdb

const uint64_t ctInputEventHandler = 0xfc0edefcebcb5878;
const uint64_t ctExitRequestedEvent = 0x824517eb48d5c653;
const uint64_t ctInputEvents = 0xe4757484f00e80ed;
