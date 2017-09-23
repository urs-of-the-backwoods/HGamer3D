#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "AlignmentCbor.hpp"

namespace cbd {

void readHorizontalAlignment(CborValue *it, HorizontalAlignment *horizontalAlignment) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    horizontalAlignment->selector = (EnumHorizontalAlignment)i;
    if (horizontalAlignment->selector == 0) {
    };
    if (horizontalAlignment->selector == 1) {
    };
    if (horizontalAlignment->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writeHorizontalAlignment(CborEncoder *enc, HorizontalAlignment horizontalAlignment) {
    if (horizontalAlignment.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)horizontalAlignment.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (horizontalAlignment.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)horizontalAlignment.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (horizontalAlignment.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)horizontalAlignment.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readVerticalAlignment(CborValue *it, VerticalAlignment *verticalAlignment) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    verticalAlignment->selector = (EnumVerticalAlignment)i;
    if (verticalAlignment->selector == 0) {
    };
    if (verticalAlignment->selector == 1) {
    };
    if (verticalAlignment->selector == 2) {
    };
cbor_value_leave_container(ita, it); }
}

void writeVerticalAlignment(CborEncoder *enc, VerticalAlignment verticalAlignment) {
    if (verticalAlignment.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)verticalAlignment.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (verticalAlignment.selector == 1) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)verticalAlignment.selector);
cbor_encoder_close_container_checked(enca, enc); }
    if (verticalAlignment.selector == 2) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
        cbor_encode_uint(enc, (uint64_t)verticalAlignment.selector);
cbor_encoder_close_container_checked(enca, enc); }
}

void readAlignment(CborValue *it, Alignment *alignment) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    readHorizontalAlignment(it, &(alignment->horizontal));
    readVerticalAlignment(it, &(alignment->vertical));
cbor_value_leave_container(ita, it); }
}

void writeAlignment(CborEncoder *enc, Alignment alignment) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    writeHorizontalAlignment(enc, alignment.horizontal);
    writeVerticalAlignment(enc, alignment.vertical);
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctAlignment = 0x1cdc5b0a65479346;
