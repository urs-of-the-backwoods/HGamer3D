#ifndef __EntityId_cbor__
#define __EntityId_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::vector<uint8_t> EntityId;

void readEntityId(CborValue *it, EntityId *entityId);
void writeEntityId(CborEncoder *enc, EntityId entityId);

} // end of namespacd cdb

extern const uint64_t ctEntityId;
#endif
