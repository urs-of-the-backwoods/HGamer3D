//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/CamerCbor.cpp

#include "CameraCbor.hpp"

namespace cbd {

void readCamera(CborValue *it0, Camera *camera)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    camera->selector = (EnumCamera)i;
    if (camera->selector == 0) {
    };
    if (camera->selector == 1) {
        readScreenRect(it, &(camera->data.OverlayCamera.value0));
        cbor_value_get_float(it, &(camera->data.OverlayCamera.value1)); cbor_value_advance_fixed(it);
    };
    cbor_value_leave_container(it0, it);
}

void writeCamera(CborEncoder *enc0, Camera camera)
{
    if (camera.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)camera.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (camera.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
        cbor_encode_uint(enc, (uint64_t)camera.selector);
        writeScreenRect(enc, camera.data.OverlayCamera.value0);
        cbor_encode_float(enc, camera.data.OverlayCamera.value1);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readFrustum(CborValue *it0, Frustum *frustum)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_float(it, &(frustum->nearDistance)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(frustum->farDistance)); cbor_value_advance_fixed(it);
    readAngle(it, &(frustum->fieldOfViewHorizontal));
    cbor_value_leave_container(it0, it);
}

void writeFrustum(CborEncoder *enc0, Frustum frustum)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    cbor_encode_float(enc, frustum.nearDistance);
    cbor_encode_float(enc, frustum.farDistance);
    writeAngle(enc, frustum.fieldOfViewHorizontal);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctCamera = 0xd3b0d455ab1f4716;
const uint64_t ctFrustum = 0xf3ce3235d4f8e73d;
