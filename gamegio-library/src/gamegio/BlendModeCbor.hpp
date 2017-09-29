#ifndef __BlendMode_cbor__
#define __BlendMode_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    BMReplace = 0,
    BMAdd = 1,
    BMMultiply = 2,
    BMAlpha = 3,
    BMAddAlpha = 4,
    BMPremulAlpha = 5,
    BMInvDestAlpha = 6,
    BMSubtract = 7,
    BMSubtractAlpha = 8,
} EnumBlendMode;

typedef struct {
    EnumBlendMode selector;
    struct {
        struct {
        } BMReplace;
        struct {
        } BMAdd;
        struct {
        } BMMultiply;
        struct {
        } BMAlpha;
        struct {
        } BMAddAlpha;
        struct {
        } BMPremulAlpha;
        struct {
        } BMInvDestAlpha;
        struct {
        } BMSubtract;
        struct {
        } BMSubtractAlpha;
    } data;
} BlendMode;

void readBlendMode(CborValue *it, BlendMode *blendMode);
void writeBlendMode(CborEncoder *enc, BlendMode blendMode);

} // end of namespacd cdb

extern const uint64_t ctBlendMode;
#endif
