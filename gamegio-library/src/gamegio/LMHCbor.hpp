#ifndef __LMH_cbor__
#define __LMH_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Low = 0,
    Medium = 1,
    High = 2,
} EnumLMH;

typedef struct {
    EnumLMH selector;
    struct {
        struct {
        } low;
        struct {
        } medium;
        struct {
        } high;
    } data;
} LMH;

void readLMH(CborValue *it0, LMH *lMH);
void writeLMH(CborEncoder *enc0, LMH lMH);

} // end of namespacd cdb

extern const uint64_t ctLMH;
#endif
