#ifndef __Name_cbor__
#define __Name_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string Name;

void readName(CborValue *it, Name *name);
void writeName(CborEncoder *enc, Name name);

} // end of namespacd cdb

extern const uint64_t ctName;
#endif
