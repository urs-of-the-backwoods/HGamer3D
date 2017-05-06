#ifndef __UnitQuaternion_cbor__
#define __UnitQuaternion_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    float w;
    float x;
    float y;
    float z;
} UnitQuaternion;

typedef UnitQuaternion Orientation;

void readUnitQuaternion(CborValue *it0, UnitQuaternion *unitQuaternion);
void writeUnitQuaternion(CborEncoder *enc0, UnitQuaternion unitQuaternion);
void readOrientation(CborValue *it, Orientation *orientation);
void writeOrientation(CborEncoder *enc, Orientation orientation);

} // end of namespacd cdb

extern const uint64_t ctOrientation;
#endif
