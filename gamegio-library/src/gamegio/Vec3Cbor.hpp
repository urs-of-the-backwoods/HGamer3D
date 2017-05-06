#ifndef __Vec3_cbor__
#define __Vec3_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    float x;
    float y;
    float z;
} Vec3;

typedef Vec3 Position;

typedef Vec3 Scale;

void readVec3(CborValue *it0, Vec3 *vec3);
void writeVec3(CborEncoder *enc0, Vec3 vec3);
void readPosition(CborValue *it, Position *position);
void writePosition(CborEncoder *enc, Position position);
void readScale(CborValue *it, Scale *scale);
void writeScale(CborEncoder *enc, Scale scale);

} // end of namespacd cdb

extern const uint64_t ctPosition;
extern const uint64_t ctScale;
#endif
