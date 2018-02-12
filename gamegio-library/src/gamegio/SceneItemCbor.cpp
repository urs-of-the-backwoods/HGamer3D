#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "SceneItemCbor.hpp"

namespace cbd {

void readScene(CborValue *it, Scene *scene) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    scene->selector = (EnumScene)i;
    if (scene->selector == 0) {
        { size_t l; cbor_value_calculate_string_length(it, &l); scene->data.XmlScene.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(scene->data.XmlScene.value0.c_str()), &l, NULL); cbor_value_advance(it);}
    };
    if (scene->selector == 1) {
        { size_t l; cbor_value_calculate_string_length(it, &l); scene->data.BinaryScene.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(scene->data.BinaryScene.value0.c_str()), &l, NULL); cbor_value_advance(it);}
    };
cbor_value_leave_container(ita, it); }
}

void writeScene(CborEncoder *enc, Scene scene) {
    if (scene.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)scene.selector);
        cbor_encode_text_stringz(enc, scene.data.XmlScene.value0.c_str());
cbor_encoder_close_container_checked(enca, enc); }
    if (scene.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)scene.selector);
        cbor_encode_text_stringz(enc, scene.data.BinaryScene.value0.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctScene = 0x829863cdd141007e;
