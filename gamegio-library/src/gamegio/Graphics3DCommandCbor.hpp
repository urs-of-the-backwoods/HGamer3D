#ifndef __Graphics3DCommand_cbor__
#define __Graphics3DCommand_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    NoCmd = 0,
    Step = 1,
} EnumGraphics3DCommand;

typedef struct {
    EnumGraphics3DCommand selector;
    struct {
        struct {
        } noCmd;
        struct {
        } step;
    } data;
} Graphics3DCommand;

void readGraphics3DCommand(CborValue *it0, Graphics3DCommand *graphics3DCommand);
void writeGraphics3DCommand(CborEncoder *enc0, Graphics3DCommand graphics3DCommand);

} // end of namespacd cdb

extern const uint64_t ctGraphics3DCommand;
#endif
