#include "LayoutCbor.hpp"

namespace cbd {

void readLayoutMode(CborValue *it, LayoutMode *layoutMode) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    layoutMode->selector = (EnumLayoutMode)i;
    if (layoutMode->selector == 0) {
    };
    if (layoutMode->selector == 1) {
    };
    if (layoutMode->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writeLayoutMode(CborEncoder *enc, LayoutMode layoutMode) {
    if (layoutMode.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)layoutMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (layoutMode.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)layoutMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (layoutMode.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)layoutMode.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readLayout(CborValue *it, Layout *layout) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    readLayoutMode(it, &(layout->mode));
    { int i; cbor_value_get_int(it, &i); layout->spacing = (int32_t)i;} cbor_value_advance_fixed(it);
    readScreenRect2(it, &(layout->borders));
cbor_value_leave_container(ita, it); }
}

void writeLayout(CborEncoder *enc, Layout layout) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 3);
    writeLayoutMode(enc, layout.mode);
    cbor_encode_int(enc, (int64_t)layout.spacing);
    writeScreenRect2(enc, layout.borders);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctLayout = 0x1c94738af20a0ac6;
