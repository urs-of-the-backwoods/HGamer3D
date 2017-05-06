#include "PlayCmdCbor.hpp"

namespace cbd {

void readPlayCmd(CborValue *it0, PlayCmd *playCmd)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    playCmd->selector = (EnumPlayCmd)i;
    if (playCmd->selector == 0) {
    };
    if (playCmd->selector == 1) {
    };
    if (playCmd->selector == 2) {
    };
    cbor_value_leave_container(it0, it);
}

void writePlayCmd(CborEncoder *enc0, PlayCmd playCmd)
{
    if (playCmd.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)playCmd.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (playCmd.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)playCmd.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (playCmd.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)playCmd.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctPlayCmd = 0x35f7752020f7f1cd;
