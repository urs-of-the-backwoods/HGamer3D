#ifndef __Texture_cbor__
#define __Texture_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string Texture;

void readTexture(CborValue *it, Texture *texture);
void writeTexture(CborEncoder *enc, Texture texture);

} // end of namespacd cdb

extern const uint64_t ctTexture;
#endif
