#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

#include "VolumeCbor.hpp"

namespace cbd {

void readVolume(CborValue *it, Volume *volume) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    { size_t l; cbor_value_calculate_string_length(it, &l); volume->group.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(volume->group.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_get_float(it, &(volume->gain)); cbor_value_advance_fixed(it);
cbor_value_leave_container(ita, it); }
}

void writeVolume(CborEncoder *enc, Volume volume) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    cbor_encode_text_stringz(enc, volume.group.c_str());
    cbor_encode_float(enc, volume.gain);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctVolume = 0x659d20e6e65f85fe;