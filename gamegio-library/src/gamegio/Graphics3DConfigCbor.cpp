#include "Graphics3DConfigCbor.hpp"

namespace cbd {

void readEngineConfig(CborValue *it0, EngineConfig *engineConfig)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    cbor_value_get_boolean(it, &(engineConfig->headless)); cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(engineConfig->flushGPU)); cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(engineConfig->threads)); cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(engineConfig->forceGL2)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeEngineConfig(CborEncoder *enc0, EngineConfig engineConfig)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    cbor_encode_boolean(enc, engineConfig.headless);
    cbor_encode_boolean(enc, engineConfig.flushGPU);
    cbor_encode_boolean(enc, engineConfig.threads);
    cbor_encode_boolean(enc, engineConfig.forceGL2);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readLogLevel(CborValue *it0, LogLevel *logLevel)
{
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    logLevel->selector = (EnumLogLevel)i;
    if (logLevel->selector == 0) {
    };
    if (logLevel->selector == 1) {
    };
    if (logLevel->selector == 2) {
    };
    cbor_value_leave_container(it0, it);
}

void writeLogLevel(CborEncoder *enc0, LogLevel logLevel)
{
    if (logLevel.selector == 0) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)logLevel.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (logLevel.selector == 1) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)logLevel.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
    if (logLevel.selector == 2) {
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
        cbor_encode_uint(enc, (uint64_t)logLevel.selector);
        cbor_encoder_close_container_checked(enc0, enc);
    };
}

void readLogging(CborValue *it0, Logging *logging)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    readLogLevel(it, &(logging->logLevel));
    cbor_value_get_boolean(it, &(logging->quietLogging)); cbor_value_advance_fixed(it);
    { size_t l; cbor_value_calculate_string_length(it, &l); logging->logFileName.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(logging->logFileName.c_str()), &l, NULL); cbor_value_advance(it);}
    cbor_value_leave_container(it0, it);
}

void writeLogging(CborEncoder *enc0, Logging logging)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    writeLogLevel(enc, logging.logLevel);
    cbor_encode_boolean(enc, logging.quietLogging);
    cbor_encode_text_stringz(enc, logging.logFileName.c_str());
    cbor_encoder_close_container_checked(enc0, enc);
}

void readWindowG3D(CborValue *it0, WindowG3D *windowG3D)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    { int i; cbor_value_get_int(it, &i); windowG3D->width = (int32_t)i;} cbor_value_advance_fixed(it);
    { int i; cbor_value_get_int(it, &i); windowG3D->height = (int32_t)i;} cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(windowG3D->borderless)); cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(windowG3D->fullScreen)); cbor_value_advance_fixed(it);
    cbor_value_get_boolean(it, &(windowG3D->resizable)); cbor_value_advance_fixed(it);
    cbor_value_leave_container(it0, it);
}

void writeWindowG3D(CborEncoder *enc0, WindowG3D windowG3D)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 5);
    cbor_encode_int(enc, (int64_t)windowG3D.width);
    cbor_encode_int(enc, (int64_t)windowG3D.height);
    cbor_encode_boolean(enc, windowG3D.borderless);
    cbor_encode_boolean(enc, windowG3D.fullScreen);
    cbor_encode_boolean(enc, windowG3D.resizable);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readGraphicsQuality(CborValue *it0, GraphicsQuality *graphicsQuality)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    readLMH(it, &(graphicsQuality->shadow));
    readLMH(it, &(graphicsQuality->material));
    readLMH(it, &(graphicsQuality->texture));
    readLMH(it, &(graphicsQuality->multisample));
    cbor_value_leave_container(it0, it);
}

void writeGraphicsQuality(CborEncoder *enc0, GraphicsQuality graphicsQuality)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    writeLMH(enc, graphicsQuality.shadow);
    writeLMH(enc, graphicsQuality.material);
    writeLMH(enc, graphicsQuality.texture);
    writeLMH(enc, graphicsQuality.multisample);
    cbor_encoder_close_container_checked(enc0, enc);
}

void readGraphics3DConfig(CborValue *it0, Graphics3DConfig *graphics3DConfig)
{
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);
    readEngineConfig(it, &(graphics3DConfig->engine));
    readGraphicsQuality(it, &(graphics3DConfig->quality));
    readLogging(it, &(graphics3DConfig->logging));
    readWindowG3D(it, &(graphics3DConfig->window));
    cbor_value_leave_container(it0, it);
}

void writeGraphics3DConfig(CborEncoder *enc0, Graphics3DConfig graphics3DConfig)
{
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 4);
    writeEngineConfig(enc, graphics3DConfig.engine);
    writeGraphicsQuality(enc, graphics3DConfig.quality);
    writeLogging(enc, graphics3DConfig.logging);
    writeWindowG3D(enc, graphics3DConfig.window);
    cbor_encoder_close_container_checked(enc0, enc);
}


} // end of namespacd cdb

const uint64_t ctGraphics3DConfig = 0x884eb62b6674bff;
