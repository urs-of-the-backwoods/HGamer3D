#include "SliderCbor.hpp"

namespace cbd {

void readSlider(CborValue *it0, Slider *slider)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_float(it, &(slider->range)); cbor_value_advance_fixed(it);
    cbor_value_get_float(it, &(slider->value)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeSlider(CborEncoder *enc0, Slider slider)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
    cbor_encode_float(enc, slider.range);
    cbor_encode_float(enc, slider.value);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctSlider = 0x60636b107c77a533;
