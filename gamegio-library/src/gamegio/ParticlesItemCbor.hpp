#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    ParticleEffectResource = 0,
} EnumParticles;

typedef struct {
    EnumParticles selector;
    struct {
        struct {
            std::string value0;
        } ParticleEffectResource;
    } data;
} Particles;

void readParticles(CborValue *it, Particles *particles);
void writeParticles(CborEncoder *enc, Particles particles);

} // end of namespacd cdb

extern const uint64_t ctParticles;
