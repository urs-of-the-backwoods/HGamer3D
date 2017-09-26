#include "NameCbor.hpp"

namespace cbd {

void readName(CborValue *it, Name *name) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *name = rval;
}

void writeName(CborEncoder *enc, Name name) {
    cbor_encode_text_stringz(enc, name.c_str());
}


} // end of namespacd cdb

const uint64_t ctName = 0xf98939ae4b0d693a;
