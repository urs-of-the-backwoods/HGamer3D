#ifndef __skybox_item_cbor__
#define __skybox_item_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    SkyboxMaterial = 0,
} EnumSkybox;

typedef struct {
    EnumSkybox selector;
    struct {
        struct {
            std::string value0;
        } SkyboxMaterial;
    } data;
} Skybox;

void readSkybox(CborValue *it, Skybox *skybox);
void writeSkybox(CborEncoder *enc, Skybox skybox);

} // end of namespacd cdb

extern const uint64_t ctSkybox;

#endif
