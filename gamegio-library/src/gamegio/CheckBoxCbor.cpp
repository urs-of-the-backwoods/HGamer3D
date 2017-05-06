#include "CheckBoxCbor.hpp"

namespace cbd {

void readCheckBox(CborValue *it, CheckBox *checkBox)
{
    bool rval;
    cbor_value_get_boolean(it, &(rval)); cbor_value_advance_fixed(it);
    *checkBox = rval;
}

void writeCheckBox(CborEncoder *enc, CheckBox checkBox)
{
    cbor_encode_boolean(enc, checkBox);
}


} // end of namespacd cdb

const uint64_t ctCheckBox = 0xd2425f880fcdd9a4;
