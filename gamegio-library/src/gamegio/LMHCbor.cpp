#include "LMHCbor.hpp"

namespace cbd {

void readLMH(CborValue *it0, LMH *lMH)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    lMH->selector = (EnumLMH)i;
    if (lMH->selector == 0) {
    };
    if (lMH->selector == 1) {
    };
    if (lMH->selector == 2) {
    };
    cbor_value_leave_container(it0, it);
}

void writeLMH(CborEncoder *enc0, LMH lMH)
{
    if (lMH.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)lMH.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (lMH.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)lMH.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (lMH.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)lMH.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctLMH = 0xd632bb5447a6c93c;
