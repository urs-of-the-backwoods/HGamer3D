#ifndef __Volume_cbor__
#define __Volume_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    std::string group;
    float gain;
} Volume;

void readVolume(CborValue *it0, Volume *volume);
void writeVolume(CborEncoder *enc0, Volume volume);

} // end of namespacd cdb

extern const uint64_t ctVolume;
#endif
