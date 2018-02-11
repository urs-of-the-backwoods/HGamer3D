#include "KeyEventCbor.hpp"

namespace cbd {

void readKeyData(CborValue *it0, KeyData *keyData)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { int i; cbor_value_get_int(it, &i); keyData->key = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); keyData->scancode = (int32_t)i;} cbor_value_advance_fixed(it);
    { size_t l; cbor_value_calculate_string_length(it, &l); keyData->name.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(keyData->name.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_leave_container(it0, it);
}

void writeKeyData(CborEncoder *enc0, KeyData keyData)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    cbor_encode_int(enc, (int64_t)keyData.key);
    cbor_encode_int(enc, (int64_t)keyData.scancode);
    cbor_encode_text_stringz(enc, keyData.name.c_str());
    cbor_encoder_close_container_checked(enc0, enc);
}

void readKeyEvent(CborValue *it0, KeyEvent *keyEvent)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    keyEvent->selector = (EnumKeyEvent)i;
    if (keyEvent->selector == 0) {
    };
    if (keyEvent->selector == 1) {
        readKeyData(it, &(keyEvent->data.KeyUpEvent.value0));
    };
    if (keyEvent->selector == 2) {
        readKeyData(it, &(keyEvent->data.KeyDownEvent.value0));
    };
    cbor_value_leave_container(it0, it);
}

void writeKeyEvent(CborEncoder *enc0, KeyEvent keyEvent)
{
    if (keyEvent.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)keyEvent.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (keyEvent.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)keyEvent.selector);
        writeKeyData(enc, keyEvent.data.KeyUpEvent.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (keyEvent.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)keyEvent.selector);
        writeKeyData(enc, keyEvent.data.KeyDownEvent.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctKeyEvent090 = 0x5ba1617fb50e97e5;
const uint64_t ctKeyEvent = 0x07995a794e698f4b;
