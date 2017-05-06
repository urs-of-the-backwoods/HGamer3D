#ifndef __Light_cbor__
#define __Light_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

#include "AngleCbor.hpp"
namespace cbd {

typedef enum {
    PointLight = 0,
    DirectionalLight = 1,
    SpotLight = 2,
} EnumLightType;

typedef struct {
    EnumLightType selector;
    struct {
        struct {
        } PointLight;
        struct {
        } DirectionalLight;
        struct {
            Angle value0;
            float value1;
        } SpotLight;
    } data;
} LightType;

typedef struct {
    LightType type;
    float brightness;
    float range;
    float specularIntensity;
} Light;

void readLightType(CborValue *it0, LightType *lightType);
void writeLightType(CborEncoder *enc0, LightType lightType);
void readLight(CborValue *it0, Light *light);
void writeLight(CborEncoder *enc0, Light light);

} // end of namespacd cdb

extern const uint64_t ctLight;
#endif
