#ifndef __StaticText_cbor__
#define __StaticText_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string StaticText;

void readStaticText(CborValue *it, StaticText *staticText);
void writeStaticText(CborEncoder *enc, StaticText staticText);

} // end of namespacd cdb

extern const uint64_t ctStaticText;
#endif
