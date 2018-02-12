#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "LabelCbor.hpp"

namespace cbd {

void readLabel(CborValue *it, Label *label) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *label = rval;
}

void writeLabel(CborEncoder *enc, Label label) {
    cbor_encode_text_stringz(enc, label.c_str());
}


} // end of namespacd cdb

const uint64_t ctLabel = 0xf98939ae4b0d693a;
