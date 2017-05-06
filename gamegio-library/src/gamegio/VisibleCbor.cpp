#include "VisibleCbor.hpp"

namespace cbd {

void readVisible(CborValue *it, Visible *visible)
{
    bool rval;
    cbor_value_get_boolean(it, &(rval)); cbor_value_advance_fixed(it);
    *visible = rval;
}

void writeVisible(CborEncoder *enc, Visible visible)
{
    cbor_encode_boolean(enc, visible);
}


} // end of namespacd cdb

const uint64_t ctVisible = 0x98e7a78e949e1c6e;
