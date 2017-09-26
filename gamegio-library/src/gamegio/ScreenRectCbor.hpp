#ifndef __ScreenRect_cbor__
#define __ScreenRect_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    int32_t x;
    int32_t y;
    int32_t width;
    int32_t height;
} ScreenRect;

typedef struct {
    int32_t left;
    int32_t top;
    int32_t right;
    int32_t bottom;
} ScreenRect2;

void readScreenRect(CborValue *it, ScreenRect *screenRect);
void writeScreenRect(CborEncoder *enc, ScreenRect screenRect);
void readScreenRect2(CborValue *it, ScreenRect2 *screenRect2);
void writeScreenRect2(CborEncoder *enc, ScreenRect2 screenRect2);

} // end of namespacd cdb

extern const uint64_t ctScreenRect;
extern const uint64_t ctScreenRect2;
#endif
