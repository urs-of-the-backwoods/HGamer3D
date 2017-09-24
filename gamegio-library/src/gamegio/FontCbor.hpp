#ifndef __Font_cbor__
#define __Font_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    std::string typeface;
    int32_t size;
} Font;

void readFont(CborValue *it, Font *font);
void writeFont(CborEncoder *enc, Font font);

} // end of namespacd cdb

extern const uint64_t ctFont;
#endif
