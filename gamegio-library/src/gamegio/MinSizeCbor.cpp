#include "MinSizeCbor.hpp"

namespace cbd {

void readMinSize(CborValue *it, MinSize *minSize) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); minSize->minWidth = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); minSize->minHeight = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeMinSize(CborEncoder *enc, MinSize minSize) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    cbor_encode_int(enc, (int64_t)minSize.minWidth);
    cbor_encode_int(enc, (int64_t)minSize.minHeight);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctMinSize = 0x7534a286d000125c;
