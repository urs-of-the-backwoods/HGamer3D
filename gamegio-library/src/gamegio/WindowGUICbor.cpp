#include "WindowGUICbor.hpp"

namespace cbd {

void readWindowGUI(CborValue *it, WindowGUI *windowGUI) {
    { uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);
}

void writeWindowGUI(CborEncoder *enc, WindowGUI windowGUI) {
    cbor_encode_simple_value(enc, NullValue);
}


} // end of namespacd cdb

const uint64_t ctWindowGUI = 0x39b4f64b33f5cb41;
