#include "MaterialCbor.hpp"

namespace cbd {

void readMaterial(CborValue *it0, Material *material)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    material->selector = (EnumMaterial)i;
    if (material->selector == 0) {
        { size_t l; cbor_value_calculate_string_length(it, &l); material->data.ResourceMaterial.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(material->data.ResourceMaterial.value0.c_str()), &l, NULL); cbor_value_advance(it);}
    };
    cbor_value_leave_container(it0, it);
}

void writeMaterial(CborEncoder *enc0, Material material)
{
    if (material.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)material.selector);
        cbor_encode_text_stringz(enc, material.data.ResourceMaterial.value0.c_str());
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctMaterial = 0xb4bae8b0d0d8c162;
