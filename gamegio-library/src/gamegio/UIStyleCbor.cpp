#include "UIStyleCbor.hpp"

namespace cbd {

void readUIStyle(CborValue *it, UIStyle *uIStyle) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *uIStyle = rval;
}

void writeUIStyle(CborEncoder *enc, UIStyle uIStyle) {
    cbor_encode_text_stringz(enc, uIStyle.c_str());
}


} // end of namespacd cdb

const uint64_t ctUIStyle = 0xbd86f89ddca9280f;
