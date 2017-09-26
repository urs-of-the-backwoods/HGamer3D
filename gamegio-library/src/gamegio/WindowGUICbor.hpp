#ifndef __WindowGUI_cbor__
#define __WindowGUI_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {} WindowGUI;

void readWindowGUI(CborValue *it, WindowGUI *windowGUI);
void writeWindowGUI(CborEncoder *enc, WindowGUI windowGUI);

} // end of namespacd cdb

extern const uint64_t ctWindowGUI;
#endif
