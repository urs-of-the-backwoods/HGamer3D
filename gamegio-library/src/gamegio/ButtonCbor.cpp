#include "ButtonCbor.hpp"

namespace cbd {

void readButton(CborValue *it, Button *button) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    cbor_value_get_boolean(it, &(button->pressed)); cbor_value_advance_fixed(it);
    { size_t l; cbor_value_calculate_string_length(it, &l); button->label.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(button->label.c_str()), &l, NULL); cbor_value_advance(it);}
cbor_value_leave_container(ita, it); }
}

void writeButton(CborEncoder *enc, Button button) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    cbor_encode_boolean(enc, button.pressed);
    cbor_encode_text_stringz(enc, button.label.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}

void readButtonEvent(CborValue *it, ButtonEvent *buttonEvent) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    buttonEvent->selector = (EnumButtonEvent)i;
    if (buttonEvent->selector == 0) {
    };
    if (buttonEvent->selector == 1) {
    };
    if (buttonEvent->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writeButtonEvent(CborEncoder *enc, ButtonEvent buttonEvent) {
    if (buttonEvent.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)buttonEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (buttonEvent.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)buttonEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (buttonEvent.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)buttonEvent.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readBasicButton(CborValue *it, BasicButton *basicButton) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *basicButton = rval;
}

void writeBasicButton(CborEncoder *enc, BasicButton basicButton) {
    cbor_encode_text_stringz(enc, basicButton.c_str());
}

void readImageButton(CborValue *it, ImageButton *imageButton) {
    std::string rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(rval.c_str()), &l, NULL); cbor_value_advance(it);}
    *imageButton = rval;
}

void writeImageButton(CborEncoder *enc, ImageButton imageButton) {
    cbor_encode_text_stringz(enc, imageButton.c_str());
}


} // end of namespacd cdb

const uint64_t ctButton = 0x68a1857c27690b30;
const uint64_t ctButtonEvent = 0x3049202a50c414a7;
const uint64_t ctBasicButton = 0x372110c1ae4548b8;
const uint64_t ctImageButton = 0x916f39acd0fc989e;
