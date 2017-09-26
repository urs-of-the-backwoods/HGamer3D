#include "TooltipCbor.hpp"

namespace cbd {

void readTooltip(CborValue *it, Tooltip *tooltip) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *tooltip = rval;
}

void writeTooltip(CborEncoder *enc, Tooltip tooltip) {
    cbor_encode_text_stringz(enc, tooltip.c_str());
}


} // end of namespacd cdb

const uint64_t ctTooltip = 0x620bb5dd7dfca052;
