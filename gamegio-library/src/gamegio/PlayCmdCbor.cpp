#include "PlayCmdCbor.hpp"

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

void readPlayCmd(CborValue *it, PlayCmd *playCmd) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    playCmd->selector = (EnumPlayCmd)i;
    if (playCmd->selector == 0) {
    };
    if (playCmd->selector == 1) {
    };
    if (playCmd->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writePlayCmd(CborEncoder *enc, PlayCmd playCmd) {
    if (playCmd.selector == 0)
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)playCmd.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (playCmd.selector == 1)
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)playCmd.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (playCmd.selector == 2)
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)playCmd.selector);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctPlayCmd = 0x35f7752020f7f1cd;

