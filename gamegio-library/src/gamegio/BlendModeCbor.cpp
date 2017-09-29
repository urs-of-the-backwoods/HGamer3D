#include "BlendModeCbor.hpp"

namespace cbd {

void readBlendMode(CborValue *it, BlendMode *blendMode) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    blendMode->selector = (EnumBlendMode)i;
    if (blendMode->selector == 0) {
    };
    if (blendMode->selector == 1) {
    };
    if (blendMode->selector == 2) {
    };
    if (blendMode->selector == 3) {
    };
    if (blendMode->selector == 4) {
    };
    if (blendMode->selector == 5) {
    };
    if (blendMode->selector == 6) {
    };
    if (blendMode->selector == 7) {
    };
    if (blendMode->selector == 8) {
    };
cbor_value_leave_container(ita, it); }
}

void writeBlendMode(CborEncoder *enc, BlendMode blendMode) {
    if (blendMode.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 5) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 6) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 7) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (blendMode.selector == 8) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)blendMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctBlendMode = 0xe010f1bdee3d43a9;
