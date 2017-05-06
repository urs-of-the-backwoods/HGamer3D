#include "ColourCbor.hpp"

namespace cbd {

void readColour(CborValue *it0, Colour *colour)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_float(it, &(colour->red)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(colour->green)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(colour->blue)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(colour->alpha)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeColour(CborEncoder *enc0, Colour colour)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    cbor_encode_float(enc, colour.red);
    cbor_encode_float(enc, colour.green);
    cbor_encode_float(enc, colour.blue);
    cbor_encode_float(enc, colour.alpha);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctColour = 0xe202add0521cde41;
