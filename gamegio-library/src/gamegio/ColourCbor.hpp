#ifndef __Colour_cbor__
#define __Colour_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    float red;
    float green;
    float blue;
    float alpha;
} Colour;

void readColour(CborValue *it0, Colour *colour);
void writeColour(CborEncoder *enc0, Colour colour);

} // end of namespacd cdb

extern const uint64_t ctColour;
#endif
