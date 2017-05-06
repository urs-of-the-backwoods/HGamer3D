#ifndef __ScreenRect_cbor__
#define __ScreenRect_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    int32_t x;
    int32_t y;
    int32_t width;
    int32_t height;
} ScreenRect;

void readScreenRect(CborValue *it0, ScreenRect *screenRect);
void writeScreenRect(CborEncoder *enc0, ScreenRect screenRect);

} // end of namespacd cdb

extern const uint64_t ctScreenRect;
#endif
