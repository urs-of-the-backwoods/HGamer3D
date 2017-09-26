#ifndef __MinSize_cbor__
#define __MinSize_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    int32_t minWidth;
    int32_t minHeight;
} MinSize;

void readMinSize(CborValue *it, MinSize *minSize);
void writeMinSize(CborEncoder *enc, MinSize minSize);

} // end of namespacd cdb

extern const uint64_t ctMinSize;
#endif
