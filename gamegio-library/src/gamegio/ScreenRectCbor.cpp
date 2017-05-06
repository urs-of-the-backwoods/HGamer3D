#include "ScreenRectCbor.hpp"

namespace cbd {

void readScreenRect(CborValue *it0, ScreenRect *screenRect)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { int i; cbor_value_get_int(it, &i); screenRect->x = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect->y = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect->width = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect->height = (int32_t)i;} cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeScreenRect(CborEncoder *enc0, ScreenRect screenRect)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    cbor_encode_int(enc, (int64_t)screenRect.x);
    cbor_encode_int(enc, (int64_t)screenRect.y);
    cbor_encode_int(enc, (int64_t)screenRect.width);
    cbor_encode_int(enc, (int64_t)screenRect.height);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctScreenRect = 0x16877957e32da6b1;
