#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "FontCbor.hpp"

namespace cbd {

void readFont(CborValue *it, Font *font) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *font = rval;
}

void writeFont(CborEncoder *enc, Font font) {
    cbor_encode_text_stringz(enc, font.c_str());
}


} // end of namespacd cdb

const uint64_t ctFont = 0x457ac00afe66a3a4;
