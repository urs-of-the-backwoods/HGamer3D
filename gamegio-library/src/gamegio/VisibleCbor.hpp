#ifndef __Visible_cbor__
#define __Visible_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef bool Visible;

void readVisible(CborValue *it, Visible *visible);
void writeVisible(CborEncoder *enc, Visible visible);

} // end of namespacd cdb

extern const uint64_t ctVisible;
#endif
