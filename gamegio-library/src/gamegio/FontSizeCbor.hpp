#ifndef __FontSize_cbor__
#define __FontSize_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef int32_t FontSize;

void readFontSize(CborValue *it, FontSize *fontSize);
void writeFontSize(CborEncoder *enc, FontSize fontSize);

} // end of namespacd cdb

extern const uint64_t ctFontSize;

#endif

