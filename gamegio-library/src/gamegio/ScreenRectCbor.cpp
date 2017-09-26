#include "ScreenRectCbor.hpp"

namespace cbd {

void readScreenRect(CborValue *it, ScreenRect *screenRect) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); screenRect->x = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect->y = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect->width = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect->height = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeScreenRect(CborEncoder *enc, ScreenRect screenRect) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
    cbor_encode_int(enc, (int64_t)screenRect.x);
    cbor_encode_int(enc, (int64_t)screenRect.y);
    cbor_encode_int(enc, (int64_t)screenRect.width);
    cbor_encode_int(enc, (int64_t)screenRect.height);
cbor_encoder_close_container_checked(enca, enc); }
}

void readScreenRect2(CborValue *it, ScreenRect2 *screenRect2) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { int i; cbor_value_get_int(it, &i); screenRect2->left = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect2->top = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect2->right = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); screenRect2->bottom = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeScreenRect2(CborEncoder *enc, ScreenRect2 screenRect2) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 4);
    cbor_encode_int(enc, (int64_t)screenRect2.left);
    cbor_encode_int(enc, (int64_t)screenRect2.top);
    cbor_encode_int(enc, (int64_t)screenRect2.right);
    cbor_encode_int(enc, (int64_t)screenRect2.bottom);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctScreenRect = 0x16877957e32da6b1;
const uint64_t ctScreenRect2 = 0x5009dcc85ea5f959;
