#ifndef __Tooltip_cbor__
#define __Tooltip_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string Tooltip;

void readTooltip(CborValue *it, Tooltip *tooltip);
void writeTooltip(CborEncoder *enc, Tooltip tooltip);

} // end of namespacd cdb

extern const uint64_t ctTooltip;
#endif
