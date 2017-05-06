#include "UIElementCbor.hpp"

namespace cbd {

void readUIElement(CborValue *it, UIElement *uIElement)
{
    { uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);
}

void writeUIElement(CborEncoder *enc, UIElement uIElement)
{
    cbor_encode_simple_value(enc, NullValue);
}


} // end of namespacd cdb

const uint64_t ctUIElement = 0xd5b79f5837e52274;
