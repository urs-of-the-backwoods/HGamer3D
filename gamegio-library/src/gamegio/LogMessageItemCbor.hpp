#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "Graphics3DConfigCbor.hpp"

namespace cbd {

typedef struct {
    LogLevel level;
    std::string message;
} LogMessage;

void readLogMessage(CborValue *it, LogMessage *logMessage);
void writeLogMessage(CborEncoder *enc, LogMessage logMessage);

} // end of namespacd cdb

extern const uint64_t ctLogMessage;
