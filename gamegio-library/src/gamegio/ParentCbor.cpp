#include "ParentCbor.hpp"

namespace cbd {

void readParent(CborValue *it, Parent *parent)
{
    std::vector<uint8_t> rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_byte_string(it, rval.data(), &l, NULL); cbor_value_advance(it);}
    *parent = rval;
}

void writeParent(CborEncoder *enc, Parent parent)
{
    cbor_encode_byte_string(enc, parent.data(), parent.size());
}


} // end of namespacd cdb

const uint64_t ctParent = 0xbadd24df00e737d8;
