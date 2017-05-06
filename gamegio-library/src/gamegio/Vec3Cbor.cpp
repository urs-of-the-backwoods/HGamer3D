#include "Vec3Cbor.hpp"

namespace cbd {

void readVec3(CborValue *it0, Vec3 *vec3)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_float(it, &(vec3->x)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(vec3->y)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(vec3->z)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeVec3(CborEncoder *enc0, Vec3 vec3)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    cbor_encode_float(enc, vec3.x);
    cbor_encode_float(enc, vec3.y);
    cbor_encode_float(enc, vec3.z);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readPosition(CborValue *it, Position *position)
{
    Vec3 rval;
    readVec3(it, &(rval));
    *position = rval;
}

void writePosition(CborEncoder *enc, Position position)
{
    writeVec3(enc, position);
}

void readScale(CborValue *it, Scale *scale)
{
    Vec3 rval;
    readVec3(it, &(rval));
    *scale = rval;
}

void writeScale(CborEncoder *enc, Scale scale)
{
    writeVec3(enc, scale);
}


} // end of namespacd cdb

const uint64_t ctPosition = 0x29aacbbb10c84016;
const uint64_t ctScale = 0x2f9c124bc8fd41c4;
