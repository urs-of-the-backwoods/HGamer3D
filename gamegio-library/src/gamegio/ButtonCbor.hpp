#ifndef __Button_cbor__
#define __Button_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    bool pressed;
    std::string label;
} Button;

void readButton(CborValue *it0, Button *button);
void writeButton(CborEncoder *enc0, Button button);

} // end of namespacd cdb

extern const uint64_t ctButton;
#endif
