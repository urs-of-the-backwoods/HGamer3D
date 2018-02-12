#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "LogMessageItemCbor.hpp"
#include "Graphics3DConfigCbor.hpp"

namespace cbd {

void readLogMessage(CborValue *it, LogMessage *logMessage) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
    readLogLevel(it, &(logMessage->level));
    { size_t l; cbor_value_calculate_string_length(it, &l); logMessage->message.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(logMessage->message.c_str()), &l, NULL); cbor_value_advance(it);}
cbor_value_leave_container(ita, it); }
}

void writeLogMessage(CborEncoder *enc, LogMessage logMessage) {
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
    writeLogLevel(enc, logMessage.level);
    cbor_encode_text_stringz(enc, logMessage.message.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctLogMessage = 0x1c94738af20a0ac6;
