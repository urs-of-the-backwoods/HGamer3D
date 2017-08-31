#ifndef __DropDownList_cbor__
#define __DropDownList_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {                                                                                                 
                                                                                                                
typedef enum {                                                                                                  
    Selection = 0,                                                                                              
    NoSelection = 1,                                                                                            
} EnumTextSelection;                                                                                            
                                                                                                                
typedef struct {                                                                                                
    EnumTextSelection selector;                                                                                 
    struct {                                                                                                    
        struct {                                                                                                
            int32_t value0;                                                                                     
        } Selection;                                                                                            
        struct {                                                                                                
        } NoSelection;                                                                                          
    } data;                                                                                                     
} TextSelection;                                                                                                
                                                                                                                
typedef struct {                                                                                                
    std::vector<std::string> content;                                                                           
    TextSelection selected;                                                                                     
} DropDownList;                                                                                                 
                                                                                                                
void readTextSelection(CborValue *it0, TextSelection *textSelection);                                           
void writeTextSelection(CborEncoder *enc0, TextSelection textSelection);                                        
void readDropDownList(CborValue *it0, DropDownList *dropDownList);                                              
void writeDropDownList(CborEncoder *enc0, DropDownList dropDownList);                                           
                                                                                                                
} // end of namespacd cdb                                                                                       
                                                                                                                
extern const uint64_t ctDropDownList;                                                                           
                                                                                                               
#endif
