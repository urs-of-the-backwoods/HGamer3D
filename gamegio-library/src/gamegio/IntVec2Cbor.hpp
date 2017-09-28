#ifndef __IntVec2_cbor__
#define __IntVec2_cbor__

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
} IntVec2;

typedef IntVec2 Position2D;

typedef IntVec2 Size2D;

void readIntVec2(CborValue *it, IntVec2 *intVec2);
void writeIntVec2(CborEncoder *enc, IntVec2 intVec2);
void readPosition2D(CborValue *it, Position2D *position2D);
void writePosition2D(CborEncoder *enc, Position2D position2D);
void readSize2D(CborValue *it, Size2D *size2D);
void writeSize2D(CborEncoder *enc, Size2D size2D);

} // end of namespacd cdb

extern const uint64_t ctPosition2D;
extern const uint64_t ctSize2D;
#endif
