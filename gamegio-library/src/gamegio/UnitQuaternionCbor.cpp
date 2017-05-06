#include "UnitQuaternionCbor.hpp"

namespace cbd {

void readUnitQuaternion(CborValue *it0, UnitQuaternion *unitQuaternion)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_float(it, &(unitQuaternion->w)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(unitQuaternion->x)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(unitQuaternion->y)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(unitQuaternion->z)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeUnitQuaternion(CborEncoder *enc0, UnitQuaternion unitQuaternion)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    cbor_encode_float(enc, unitQuaternion.w);
    cbor_encode_float(enc, unitQuaternion.x);
    cbor_encode_float(enc, unitQuaternion.y);
    cbor_encode_float(enc, unitQuaternion.z);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readOrientation(CborValue *it, Orientation *orientation)
{
    UnitQuaternion rval;
    readUnitQuaternion(it, &(rval));
    *orientation = rval;
}

void writeOrientation(CborEncoder *enc, Orientation orientation)
{
    writeUnitQuaternion(enc, orientation);
}


} // end of namespacd cdb

const uint64_t ctOrientation = 0x815eb4d9c7bfaa74;
