#include "StaticTextCbor.hpp"
#include <string>
#include <iostream>

using namespace std;

namespace cbd {

void readStaticText(CborValue *it, StaticText *staticText)
{
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); l++; rval.resize(l);
      cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *staticText = rval;
}

void writeStaticText(CborEncoder *enc, StaticText staticText)
{
    cbor_encode_text_stringz(enc, staticText.c_str());
}


} // end of namespacd cdb

const uint64_t ctStaticText = 0xda9601eaf3319280;
