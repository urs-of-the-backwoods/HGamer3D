#ifndef __Parent_cbor__
#define __Parent_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::vector<uint8_t> Parent;

void readParent(CborValue *it, Parent *parent);
void writeParent(CborEncoder *enc, Parent parent);

} // end of namespacd cdb

extern const uint64_t ctParent;
#endif
