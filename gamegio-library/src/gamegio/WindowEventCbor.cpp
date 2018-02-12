//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/WindowEventCbor.cpp

#include "WindowEventCbor.hpp"

namespace cbd {

void readScreenModeEvent(CborValue *it, ScreenModeEvent *screenModeEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); screenModeEvent->Width = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenModeEvent->Height = (int32_t)i;} cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(screenModeEvent->Fullscreen)); cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(screenModeEvent->Borderless)); cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeScreenModeEvent(CborEncoder *enc, ScreenModeEvent screenModeEvent) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
    cbor_encode_int(enc, (int64_t)screenModeEvent.Width);
    cbor_encode_int(enc, (int64_t)screenModeEvent.Height);
    cbor_encode_boolean(enc, screenModeEvent.Fullscreen);
    cbor_encode_boolean(enc, screenModeEvent.Borderless);
cbor_encoder_close_container_checked(enca, enc); }
}

void readExitRequestedEvent(CborValue *it, ExitRequestedEvent *exitRequestedEvent) {
    struct {} rval;
    { uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);
    //*exitRequestedEvent = rval;
}

void writeExitRequestedEvent(CborEncoder *enc, ExitRequestedEvent exitRequestedEvent) {
    cbor_encode_simple_value(enc, NullValue);
}


} // end of namespacd cdb

const uint64_t ctScreenModeEvent = 0x7534a286d000125c;
const uint64_t ctExitRequestedEvent090 = 0x824517eb48d5c653;
const uint64_t ctExitRequestedEvent = 0xbd86f89ddca9280f;
