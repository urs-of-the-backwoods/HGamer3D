#include "IntVec2Cbor.hpp"

namespace cbd {

void readIntVec2(CborValue *it, IntVec2 *intVec2) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); intVec2->x = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); intVec2->y = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeIntVec2(CborEncoder *enc, IntVec2 intVec2) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    cbor_encode_int(enc, (int64_t)intVec2.x);
    cbor_encode_int(enc, (int64_t)intVec2.y);
cbor_encoder_close_container_checked(enca, enc); }
}

void readPosition2D(CborValue *it, Position2D *position2D) {
    IntVec2 rval;
    readIntVec2(it, &(rval));
    *position2D = rval;
}

void writePosition2D(CborEncoder *enc, Position2D position2D) {
    writeIntVec2(enc, position2D);
}

void readSize2D(CborValue *it, Size2D *size2D) {
    IntVec2 rval;
    readIntVec2(it, &(rval));
    *size2D = rval;
}

void writeSize2D(CborEncoder *enc, Size2D size2D) {
    writeIntVec2(enc, size2D);
}


} // end of namespacd cdb

const uint64_t ctPosition2D = 0x7995a794e698f4b;
const uint64_t ctSize2D = 0x8a73da7bdfbe4ccc;
