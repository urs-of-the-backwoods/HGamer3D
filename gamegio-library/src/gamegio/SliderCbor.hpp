#ifndef __Slider_cbor__
#define __Slider_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    float range;
    float value;
} Slider;

void readSlider(CborValue *it0, Slider *slider);
void writeSlider(CborEncoder *enc0, Slider slider);

} // end of namespacd cdb

extern const uint64_t ctSlider;
#endif
