#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::string Label;

void readLabel(CborValue *it, Label *label);
void writeLabel(CborEncoder *enc, Label label);

} // end of namespacd cdb

extern const uint64_t ctLabel;
