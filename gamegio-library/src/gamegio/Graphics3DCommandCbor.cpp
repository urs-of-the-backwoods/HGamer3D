#include "Graphics3DCommandCbor.hpp"

namespace cbd {

void readGraphics3DCommand(CborValue *it0, Graphics3DCommand *graphics3DCommand)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    graphics3DCommand->selector = (EnumGraphics3DCommand)i;
    if (graphics3DCommand->selector == 0) {
    };
    if (graphics3DCommand->selector == 1) {
    };
    cbor_value_leave_container(it0, it);
}

void writeGraphics3DCommand(CborEncoder *enc0, Graphics3DCommand graphics3DCommand)
{
    if (graphics3DCommand.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)graphics3DCommand.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (graphics3DCommand.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)graphics3DCommand.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}


} // end of namespacd cdb

const uint64_t ctGraphics3DCommand = 0xea06bc20a9334af3;
