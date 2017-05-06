#ifndef __UIElement_cbor__
#define __UIElement_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {} UIElement;

void readUIElement(CborValue *it, UIElement *uIElement);
void writeUIElement(CborEncoder *enc, UIElement uIElement);

} // end of namespacd cdb

extern const uint64_t ctUIElement;
#endif
