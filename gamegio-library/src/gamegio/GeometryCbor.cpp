#include "GeometryCbor.hpp"

namespace cbd {

void readShape(CborValue *it0, Shape *shape)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    shape->selector = (EnumShape)i;
    if (shape->selector == 0) {
    };
    if (shape->selector == 1) {
    };
    if (shape->selector == 2) {
    };
    if (shape->selector == 3) {
    };
    if (shape->selector == 4) {
    };
    if (shape->selector == 5) {
    };
    cbor_value_leave_container(it0, it);
}

void writeShape(CborEncoder *enc0, Shape shape)
{
    if (shape.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)shape.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (shape.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)shape.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (shape.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)shape.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (shape.selector == 3) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)shape.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (shape.selector == 4) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)shape.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (shape.selector == 5) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)shape.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readGeometry(CborValue *it0, Geometry *geometry)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    geometry->selector = (EnumGeometry)i;
    if (geometry->selector == 0) {
        readShape(it, &(geometry->data.ShapeGeometry.value0));
    };
    if (geometry->selector == 1) {
        { size_t l; cbor_value_calculate_string_length(it, &l); geometry->data.ResourceGeometry.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(geometry->data.ResourceGeometry.value0.c_str()), &l, NULL); cbor_value_advance(it);}
    };
    cbor_value_leave_container(it0, it);
}

void writeGeometry(CborEncoder *enc0, Geometry geometry)
{
    if (geometry.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)geometry.selector);
        writeShape(enc, geometry.data.ShapeGeometry.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (geometry.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)geometry.selector);
        cbor_encode_text_stringz(enc, geometry.data.ResourceGeometry.value0.c_str());
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctGeometry = 0xee433d1a4b964591;
const uint64_t ctGraphicsElement = 0x65114ba821671643;
