#include "TextureCbor.hpp"

namespace cbd {

void readTexture(CborValue *it, Texture *texture) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *texture = rval;
}

void writeTexture(CborEncoder *enc, Texture texture) {
    cbor_encode_text_stringz(enc, texture.c_str());
}


} // end of namespacd cdb

const uint64_t ctTexture = 0xb28a202495ec05b1;
