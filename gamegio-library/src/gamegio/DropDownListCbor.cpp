#include "DropDownListCbor.hpp"
#include <iostream>

namespace cbd {                                                                                                                             
                                                                                                                                            
void readTextSelection(CborValue *it0, TextSelection *textSelection)                                                                        
{                                                                                                                                           
    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);                                                               
    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);                                                                        
    textSelection->selector = (EnumTextSelection)i;                                                                                         
    if (textSelection->selector == 0) {                                                                                                     
        { int i; cbor_value_get_int(it, &i); textSelection->data.Selection.value0 = (int32_t)i;} cbor_value_advance_fixed(it);              
    };                                                                                                                                      
    if (textSelection->selector == 1) {                                                                                                     
    };                                                                                                                                      
    cbor_value_leave_container(it0, it);                                                                                                    
}                                                                                                                                           
                                                                                                                                            
void writeTextSelection(CborEncoder *enc0, TextSelection textSelection)                                                                     
{                                                                                                                                           
    if (textSelection.selector == 0) {                                                                                                      
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);                                                
        cbor_encode_uint(enc, (uint64_t)textSelection.selector);                                                                            
        cbor_encode_int(enc, (int64_t)textSelection.data.Selection.value0);                                                                 
        cbor_encoder_close_container_checked(enc0, enc);                                                                                    
    };                                                                                                                                      
    if (textSelection.selector == 1) {                                                                                                      
        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);                                                
        cbor_encode_uint(enc, (uint64_t)textSelection.selector);                                                                            
        cbor_encoder_close_container_checked(enc0, enc);                                                                                    
    };                                                                                                                                      
}                                                                                                                                           
                                                                                                                                            
void readDropDownList(CborValue *it0, DropDownList *dropDownList)                                                                   
{                                                                                                                                   
    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);                                                       
    {                                                                                                                               
  CborValue *it2 = it; CborValue it3; CborValue *it = &it3;                                                                         
  cbor_value_enter_container(it2, it);                                                                                              
  size_t l; cbor_value_get_array_length(it, &l);                                                                                    
  for (int i = 0; i < l; i++) {   std::string newone; { size_t l; cbor_value_calculate_string_length(it, &l); newone.resize(l+1);   
        cbor_value_copy_text_string(it, (char *)(newone.c_str()), &l, NULL); cbor_value_advance(it);}                               
; dropDownList->content.push_back(newone); }                                                                                        
  cbor_value_leave_container(it2, it); }                                                                                            
    readTextSelection(it, &(dropDownList->selected));                                                                               
    cbor_value_leave_container(it0, it);                                                                                            
}                                                                                                                                   

                                                                                                                                            
void writeDropDownList(CborEncoder *enc0, DropDownList dropDownList)                                                                        
{                                                                                                                                           
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);                                                    
    /* TBD */    writeTextSelection(enc, dropDownList.selected);                                                                            
    cbor_encoder_close_container_checked(enc0, enc);                                                                                        
}                                                                                                                                           
                                                                                                                                            
                                                                                                                                            
} // end of namespacd cdb                                                                                                                   
                                                                                                                                            
const uint64_t ctDropDownList = 0x200de0e837a8e590;                                                                                         