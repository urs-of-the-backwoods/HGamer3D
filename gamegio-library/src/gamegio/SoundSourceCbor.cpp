#include "SoundSourceCbor.hpp"
#include <iostream>

namespace cbd {

void readSoundType(CborValue *it, SoundType *soundType) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    soundType->selector = (EnumSoundType)i;
    if (soundType->selector == 0) {
    };
    if (soundType->selector == 1) {
    };
    if (soundType->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writeSoundType(CborEncoder *enc, SoundType soundType) {
    if (soundType.selector == 0)
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)soundType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (soundType.selector == 1)
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)soundType.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (soundType.selector == 2)
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)soundType.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readSoundSource(CborValue *it, SoundSource *soundSource) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    readSoundType(it, &(soundSource->type));
    { size_t l; cbor_value_calculate_string_length(it, &l); soundSource->resource.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(soundSource->resource.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_get_boolean(it, &(soundSource->loop)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(soundSource->volume)); cbor_value_advance_fixed(it);
    { size_t l; cbor_value_calculate_string_length(it, &l); soundSource->volumeGroup.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(soundSource->volumeGroup.c_str()), &l, NULL); cbor_value_advance(it);}
cbor_value_leave_container(ita, it); }
}

void writeSoundSource(CborEncoder *enc, SoundSource soundSource) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 5);
    writeSoundType(enc, soundSource.type);
    cbor_encode_text_stringz(enc, soundSource.resource.c_str());
    cbor_encode_boolean(enc, soundSource.loop);
    cbor_encode_float(enc, soundSource.volume);
    cbor_encode_text_stringz(enc, soundSource.volumeGroup.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctSoundSource = 0xafcef7aa41d88c0d;
