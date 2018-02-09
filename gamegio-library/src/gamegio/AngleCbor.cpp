//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/AngleCbor.cpp

#include "AngleCbor.hpp"

namespace cbd {

void readAngle(CborValue *it0, Angle *angle)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    angle->selector = (EnumAngle)i;
    if (angle->selector == 0) {
        cbor_value_get_float(it, &(angle->data.Rad.value0)); cbor_value_advance_fixed(it);
    };
    if (angle->selector == 1) {
        cbor_value_get_float(it, &(angle->data.Deg.value0)); cbor_value_advance_fixed(it);
    };
    cbor_value_leave_container(it0, it);
}

void writeAngle(CborEncoder *enc0, Angle angle)
{
    if (angle.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)angle.selector);
        cbor_encode_float(enc, angle.data.Rad.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (angle.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)angle.selector);
        cbor_encode_float(enc, angle.data.Deg.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

// Angle utility

float getAngleAsRadians(cbd::Angle a) {
  if (a.selector == cbd::Rad) {
    return a.data.Rad.value0;
  }
  else if (a.selector == cbd::Deg) {
    return a.data.Deg.value0 * 57.2957795;    // 180/pi;
  }
  return 0; // this will not happen
}


} // end of namespacd cdb

