#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "FontSizeCbor.hpp"

namespace cbd {

void readFontSize(CborValue *it, FontSize *fontSize) {
    int32_t rval;
    { int i; cbor_value_get_int(it, &i); rval = (int32_t)i;} cbor_value_advance_fixed(it);
    *fontSize = rval;
}

void writeFontSize(CborEncoder *enc, FontSize fontSize) {
    cbor_encode_int(enc, (int64_t)fontSize);
}


} // end of namespacd cdb

const uint64_t ctFontSize = 0x829863cdd141007e;
