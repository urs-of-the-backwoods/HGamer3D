#include "ButtonCbor.hpp"

namespace cbd {

void readButton(CborValue *it0, Button *button)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_boolean(it, &(button->pressed)); cbor_value_advance_fixed(it);
    { size_t l; cbor_value_calculate_string_length(it, &l); button->label.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(button->label.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_leave_container(it0, it);
}

void writeButton(CborEncoder *enc0, Button button)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
    cbor_encode_boolean(enc, button.pressed);
    cbor_encode_text_stringz(enc, button.label.c_str());
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctButton = 0x68a1857c27690b30;
