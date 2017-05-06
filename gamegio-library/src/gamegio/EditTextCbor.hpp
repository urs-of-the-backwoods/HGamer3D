#ifndef __EditText_cbor__
#define __EditText_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string EditText;

void readEditText(CborValue *it, EditText *editText);
void writeEditText(CborEncoder *enc, EditText editText);

} // end of namespacd cdb

extern const uint64_t ctEditText;
#endif
