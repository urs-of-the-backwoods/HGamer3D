#ifndef __SoundSource_cbor__
#define __SoundSource_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {                                                                           
                                                                                          
typedef enum {                                                                            
    Sound = 0,                                                                            
    Sound3D = 1,                                                                          
    Music = 2,                                                                            
} EnumSoundType;                                                                          
                                                                                          
typedef struct {                                                                          
    EnumSoundType selector;                                                               
    struct {                                                                              
        struct {                                                                          
        } Sound;                                                                          
        struct {                                                                          
        } Sound3D;                                                                        
        struct {                                                                          
        } Music;                                                                          
    } data;                                                                               
} SoundType;                                                                              
                                                                                          
typedef struct {                                                                          
    SoundType type;                                                                       
    std::string resource;                                                                 
    bool loop;                                                                            
    float volume;                                                                         
    std::string volumeGroup;                                                              
} SoundSource;                                                                            
                                                                                          
void readSoundType(CborValue *it, SoundType *soundType);                                  
void writeSoundType(CborEncoder *enc, SoundType soundType);                               
void readSoundSource(CborValue *it, SoundSource *soundSource);                            
void writeSoundSource(CborEncoder *enc, SoundSource soundSource);                         
                                                                                          
} // end of namespacd cdb                                                                 
                                                                                          
extern const uint64_t ctSoundSource;                                                      
#endif
