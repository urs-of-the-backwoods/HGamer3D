#ifndef __Graphics3DConfig_cbor__
#define __Graphics3DConfig_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

#include "LMHCbor.hpp"
namespace cbd {

typedef struct {
    bool headless;
    bool flushGPU;
    bool threads;
    bool forceGL2;
} EngineConfig;

typedef enum {
    Warning = 0,
    Info = 1,
    Debug = 2,
} EnumLogLevel;

typedef struct {
    EnumLogLevel selector;
    struct {
        struct {
        } warning;
        struct {
        } info;
        struct {
        } debug;
    } data;
} LogLevel;

typedef struct {
    LogLevel logLevel;
    bool quietLogging;
    std::string logFileName;
} Logging;

typedef struct {
    int32_t width;
    int32_t height;
    bool borderless;
    bool fullScreen;
    bool resizable;
} WindowG3D;

typedef struct {
    LMH shadow;
    LMH material;
    LMH texture;
    LMH multisample;
} GraphicsQuality;

typedef struct {
    EngineConfig engine;
    GraphicsQuality quality;
    Logging logging;
    WindowG3D window;
} Graphics3DConfig;

void readEngineConfig(CborValue *it0, EngineConfig *engineConfig);
void writeEngineConfig(CborEncoder *enc0, EngineConfig engineConfig);
void readLogLevel(CborValue *it0, LogLevel *logLevel);
void writeLogLevel(CborEncoder *enc0, LogLevel logLevel);
void readLogging(CborValue *it0, Logging *logging);
void writeLogging(CborEncoder *enc0, Logging logging);
void readWindowG3D(CborValue *it0, WindowG3D *windowG3D);
void writeWindowG3D(CborEncoder *enc0, WindowG3D windowG3D);
void readGraphicsQuality(CborValue *it0, GraphicsQuality *graphicsQuality);
void writeGraphicsQuality(CborEncoder *enc0, GraphicsQuality graphicsQuality);
void readGraphics3DConfig(CborValue *it0, Graphics3DConfig *graphics3DConfig);
void writeGraphics3DConfig(CborEncoder *enc0, Graphics3DConfig graphics3DConfig);

} // end of namespacd cdb

extern const uint64_t ctGraphics3DConfig;
#endif
