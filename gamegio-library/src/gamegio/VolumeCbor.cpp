#include "VolumeCbor.hpp"

namespace cbd {

void readVolume(CborValue *it0, Volume *volume)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { size_t l; cbor_value_calculate_string_length(it, &l); volume->group.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(volume->group.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_get_float(it, &(volume->gain)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeVolume(CborEncoder *enc0, Volume volume)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
    cbor_encode_text_stringz(enc, volume.group.c_str());
    cbor_encode_float(enc, volume.gain);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctVolume = 0x659d20e6e65f85fe;
