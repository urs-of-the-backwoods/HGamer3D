#include "InputEventHandlerCbor.hpp"
using namespace std;

#include <sstream>
#include <iostream>

namespace cbd {

void readInputEventType(CborValue *it0, InputEventType *inputEventType)
{
    cout<<"in IET - 1\n";
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
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
    cbor_value_leave_container(it0, it);
    cout<<"in IET - 2\n";
}

void writeInputEventType(CborEncoder *enc0, InputEventType inputEventType)
{
    if (inputEventType.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 3) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 4) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 5) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 6) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventType.selector == 7) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readInputEventHandler(CborValue *it0, InputEventHandler *inputEventHandler)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    inputEventHandler->selector = (EnumInputEventHandler)i;
    if (inputEventHandler->selector == 0) {
    };
    if (inputEventHandler->selector == 1) {
        /* TBD */    };
    cbor_value_leave_container(it0, it);
}

void writeInputEventHandler(CborEncoder *enc0, InputEventHandler inputEventHandler)
{
    if (inputEventHandler.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)inputEventHandler.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (inputEventHandler.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)inputEventHandler.selector);
        /* TBD */        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readExitRequestedEvent(CborValue *it, ExitRequestedEvent *exitRequestedEvent)
{
    { uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);
}

void writeExitRequestedEvent(CborEncoder *enc, ExitRequestedEvent exitRequestedEvent)
{
    cbor_encode_simple_value(enc, NullValue);
}


} // end of namespacd cdb

const uint64_t ctInputEventHandler = 0xfc0edefcebcb5878;
const uint64_t ctExitRequestedEvent = 0x824517eb48d5c653;
