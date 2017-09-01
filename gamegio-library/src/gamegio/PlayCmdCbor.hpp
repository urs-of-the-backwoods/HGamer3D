#ifndef __PlayCmd_cbor__
#define __PlayCmd_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Play = 0,
    Pause = 1,
    Stop = 2,
} EnumPlayCmd;

typedef struct {
    EnumPlayCmd selector;
    struct {
        struct {
        } Play;
        struct {
        } Pause;
        struct {
        } Stop;
    } data;
} PlayCmd;

void readPlayCmd(CborValue *it, PlayCmd *playCmd);
void writePlayCmd(CborEncoder *enc, PlayCmd playCmd);

} // end of namespacd cdb

extern const uint64_t ctPlayCmd;

#endif
