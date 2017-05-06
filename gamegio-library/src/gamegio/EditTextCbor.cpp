#include "EditTextCbor.hpp"

namespace cbd {

void readEditText(CborValue *it, EditText *editText)
{
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *editText = rval;
}

void writeEditText(CborEncoder *enc, EditText editText)
{
    cbor_encode_text_stringz(enc, editText.c_str());
}


} // end of namespacd cdb

const uint64_t ctEditText = 0x8c79de2199331f3a;
