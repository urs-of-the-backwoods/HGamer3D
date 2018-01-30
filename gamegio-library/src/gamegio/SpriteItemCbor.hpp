#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    std::string resource;
    float opacity;
} Sprite;

void readSprite(CborValue *it, Sprite *sprite);
void writeSprite(CborEncoder *enc, Sprite sprite);

} // end of namespacd cdb

extern const uint64_t ctSprite;
