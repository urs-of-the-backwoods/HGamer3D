#include "LightCbor.hpp"

namespace cbd {

void readLightType(CborValue *it0, LightType *lightType)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    lightType->selector = (EnumLightType)i;
    if (lightType->selector == 0) {
    };
    if (lightType->selector == 1) {
    };
    if (lightType->selector == 2) {
        readAngle(it, &(lightType->data.SpotLight.value0));
        cbor_value_get_float(it, &(lightType->data.SpotLight.value1)); cbor_value_advance_fixed(it);
    };
    cbor_value_leave_container(it0, it);
}

void writeLightType(CborEncoder *enc0, LightType lightType)
{
    if (lightType.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)lightType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (lightType.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)lightType.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (lightType.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
        cbor_encode_uint(enc, (uint64_t)lightType.selector);
        writeAngle(enc, lightType.data.SpotLight.value0);
        cbor_encode_float(enc, lightType.data.SpotLight.value1);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readLight(CborValue *it0, Light *light)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    readLightType(it, &(light->type));
    cbor_value_get_float(it, &(light->brightness)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(light->range)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(light->specularIntensity)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeLight(CborEncoder *enc0, Light light)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    writeLightType(enc, light.type);
    cbor_encode_float(enc, light.brightness);
    cbor_encode_float(enc, light.range);
    cbor_encode_float(enc, light.specularIntensity);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctLight = 0x981e80e50d994ea9;
