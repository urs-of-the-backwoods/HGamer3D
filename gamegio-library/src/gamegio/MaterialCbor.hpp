#ifndef __Material_cbor__
#define __Material_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    ResourceMaterial = 0,
} EnumMaterial;

typedef struct {
    EnumMaterial selector;
    struct {
        struct {
            std::string value0;
        } ResourceMaterial;
    } data;
} Material;

void readMaterial(CborValue *it0, Material *material);
void writeMaterial(CborEncoder *enc0, Material material);

} // end of namespacd cdb

extern const uint64_t ctMaterial;
#endif
