#include "FontCbor.hpp"

namespace cbd {

void readFont(CborValue *it, Font *font) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { size_t l; cbor_value_calculate_string_length(it, &l); font->typeface.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(font->typeface.c_str()), &l, NULL); cbor_value_advance(it);}
    { int i; cbor_value_get_int(it, &i); font->size = (int32_t)i;} cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeFont(CborEncoder *enc, Font font) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    cbor_encode_text_stringz(enc, font.typeface.c_str());
    cbor_encode_int(enc, (int64_t)font.size);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctFont = 0x457ac00afe66a3a4;
