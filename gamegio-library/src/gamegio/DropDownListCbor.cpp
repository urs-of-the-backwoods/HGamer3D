#include "DropDownListCbor.hpp"

namespace cbd {

void readMaybeInt(CborValue *it0, MaybeInt *maybeInt)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    maybeInt->selector = (EnumMaybeInt)i;
    if (maybeInt->selector == 0) {
        { int i; cbor_value_get_int(it, &i); maybeInt->data.Just.value0 = (int32_t)i;} cbor_value_advance_fixed(it);
    };
    if (maybeInt->selector == 1) {
    };
    cbor_value_leave_container(it0, it);
}

void writeMaybeInt(CborEncoder *enc0, MaybeInt maybeInt)
{
    if (maybeInt.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
        cbor_encode_uint(enc, (uint64_t)maybeInt.selector);
        cbor_encode_int(enc, (int64_t)maybeInt.data.Just.value0);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (maybeInt.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)maybeInt.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readDropDownList(CborValue *it0, DropDownList *dropDownList)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    /* TBD */    readMaybeInt(it, &(dropDownList->selected));
    cbor_value_leave_container(it0, it);
}

void writeDropDownList(CborEncoder *enc0, DropDownList dropDownList)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
    /* TBD */    writeMaybeInt(enc, dropDownList.selected);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctDropDownList = 0x200de0e837a8e590;
