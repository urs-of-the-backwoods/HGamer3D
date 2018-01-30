#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "SpriteItemCbor.hpp"

namespace cbd {

void readSprite(CborValue *it, Sprite *sprite) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { size_t l; cbor_value_calculate_string_length(it, &l); sprite->resource.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(sprite->resource.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_get_float(it, &(sprite->opacity)); cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeSprite(CborEncoder *enc, Sprite sprite) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    cbor_encode_text_stringz(enc, sprite.resource.c_str());
    cbor_encode_float(enc, sprite.opacity);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctSprite = 0x39b4f64b33f5cb41;
