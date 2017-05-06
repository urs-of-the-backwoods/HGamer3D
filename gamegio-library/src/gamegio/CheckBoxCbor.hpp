#ifndef __CheckBox_cbor__
#define __CheckBox_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef bool CheckBox;

void readCheckBox(CborValue *it, CheckBox *checkBox);
void writeCheckBox(CborEncoder *enc, CheckBox checkBox);

} // end of namespacd cdb

extern const uint64_t ctCheckBox;
#endif
