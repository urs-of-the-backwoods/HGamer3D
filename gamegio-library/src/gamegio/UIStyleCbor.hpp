#ifndef __UIStyle_cbor__
#define __UIStyle_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string UIStyle;

void readUIStyle(CborValue *it, UIStyle *uIStyle);
void writeUIStyle(CborEncoder *enc, UIStyle uIStyle);

} // end of namespacd cdb

extern const uint64_t ctUIStyle;
#endif
