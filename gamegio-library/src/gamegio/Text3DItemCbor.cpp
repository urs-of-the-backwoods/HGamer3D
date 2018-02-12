#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "Text3DItemCbor.hpp"

namespace cbd {

void readFaceCameraMode(CborValue *it, FaceCameraMode *faceCameraMode) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    faceCameraMode->selector = (EnumFaceCameraMode)i;
    if (faceCameraMode->selector == 0) {
    };
    if (faceCameraMode->selector == 1) {
    };
    if (faceCameraMode->selector == 2) {
    };
    if (faceCameraMode->selector == 3) {
    };
    if (faceCameraMode->selector == 4) {
    };
    if (faceCameraMode->selector == 5) {
    };
    if (faceCameraMode->selector == 6) {
    };
cbor_value_leave_container(ita, it); }
}

void writeFaceCameraMode(CborEncoder *enc, FaceCameraMode faceCameraMode) {
    if (faceCameraMode.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (faceCameraMode.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (faceCameraMode.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (faceCameraMode.selector == 3) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (faceCameraMode.selector == 4) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (faceCameraMode.selector == 5) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (faceCameraMode.selector == 6) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)faceCameraMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readText3D(CborValue *it, Text3D *text3D) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { size_t l; cbor_value_calculate_string_length(it, &l); text3D->Font.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(text3D->Font.c_str()), &l, NULL); cbor_value_advance(it);}
    { int i; cbor_value_get_int(it, &i); text3D->FontSize = (int32_t)i;} cbor_value_advance_fixed(it);
    readFaceCameraMode(it, &(text3D->FCMode));
    cbor_value_get_boolean(it, &(text3D->FixedScreenSize)); cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeText3D(CborEncoder *enc, Text3D text3D) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
    cbor_encode_text_stringz(enc, text3D.Font.c_str());
    cbor_encode_int(enc, (int64_t)text3D.FontSize);
    writeFaceCameraMode(enc, text3D.FCMode);
    cbor_encode_boolean(enc, text3D.FixedScreenSize);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctText3D = 0x620bb5dd7dfca052;
