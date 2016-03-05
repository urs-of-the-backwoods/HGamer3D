//	C++ part of bindings, outer interface functions
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/interface.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdio.h>


#include "graphics3d.hpp"
#include "input.hpp"
#include "gui.hpp"
#include "audio.hpp"

uint64_t id_g3dsystem =       0x0884eb62b6674bff;
uint64_t id_cmdg3d =          0xea06bc20a9334af3;

uint64_t id_camera = 0xd3b0d455ab1f4716;
uint64_t id_frustum = 0xf3ce3235d4f8e73d;

uint64_t id_light =  0x981e80e50d994ea9;
uint64_t id_geo =    0xee433d1a4b964591;

uint64_t id_pos =    0x29aacbbb10c84016;
uint64_t id_scale =  0x2f9c124bc8fd41c4;
uint64_t id_ori =    0x815eb4d9c7bfaa74;

uint64_t id_mat = 0xb4bae8b0d0d8c162;
uint64_t id_col = 0xe202add0521cde41;

uint64_t id_inputeventhandler = 0xfc0edefcebcb5878;
uint64_t id_mouse =  0xa532f43b1c1c6bc7;
uint64_t id_mouseevent = 0x27eaf3fd46595d08;
uint64_t id_keyevent = 0x5ba1617fb50e97e5;

uint64_t id_visible = 0x98e7a78e949e1c6e;

uint64_t id_screenrect = 0x16877957e32da6b1;
uint64_t id_button = 0x68a1857c27690b30;
uint64_t id_edittext = 0x8c79de2199331f3a;
uint64_t id_text = 0xda9601eaf3319280;
uint64_t id_slider = 0x60636b107c77a533;
uint64_t id_checkbox = 0xd2425f880fcdd9a4;
uint64_t id_dropdownlist = 0x200de0e837a8e590;

uint64_t id_soundsource = 0xafcef7aa41d88c0d;
uint64_t id_soundlistener = 0x7aacf4ee5bd2f958;
uint64_t id_volume = 0x659d20e6e65f85fe;
uint64_t id_playcmd = 0x35f7752020f7f1cd;

using namespace std;

//
// -----------------------
// Audio
// -----------------------
//

// messages for soundsource
extern "C" int msg_soundsource_soundsource(void* p, char* data, int len)
{
  return ((SoundSourceItem *)p)->msgSoundSource(data, len);
}
extern "C" int msg_soundsource_playcmd(void* p, char* data, int len)
{
  return ((SoundSourceItem *)p)->msgPlayCmd(data, len);
}
extern "C" int msg_soundsource_pos(void* p, char* data, int len)
{
  return ((SoundSourceItem *)p)->msgPos(data, len);
}

// messages for soundlistener
extern "C" int msg_soundlistener_soundlistener(void* p, char* data, int len)
{
  return ((SoundListenerItem *)p)->msgSoundListener(data, len);
}
extern "C" int msg_soundlistener_pos(void* p, char* data, int len)
{
  return ((SoundListenerItem *)p)->msgPos(data, len);
}

// messages for volume
extern "C" int msg_volume_volume(void* p, char* data, int len)
{
  return ((VolumeItem *)p)->msgVolume(data, len);
}



//
// -----------------------
// GUI
// -----------------------
//

// messages for button 
extern "C" int msg_button_screenrect(void* p, char* data, int len)
{
  return ((ButtonItem *)p)->msgScreenRect(data, len);
}

// messages for edittext 
extern "C" int msg_edittext_edittext(void* p, char* data, int len)
{
  return ((EditTextItem *)p)->msgEditText(data, len);
}
extern "C" int msg_edittext_screenrect(void* p, char* data, int len)
{
  return ((EditTextItem *)p)->msgScreenRect(data, len);
}

// messages for text
extern "C" int msg_text_text(void* p, char* data, int len)
{
  return ((TextItem *)p)->msgText(data, len);
}
extern "C" int msg_text_screenrect(void* p, char* data, int len)
{
  return ((TextItem *)p)->msgScreenRect(data, len);
}

// messages for slider
extern "C" int msg_slider_slider(void* p, char* data, int len)
{
  return ((SliderItem *)p)->msgSlider(data, len);
}
extern "C" int msg_slider_screenrect(void* p, char* data, int len)
{
  return ((SliderItem *)p)->msgScreenRect(data, len);
}

// messages for checkbox
extern "C" int msg_checkbox_checkbox(void* p, char* data, int len)
{
  return ((CheckBoxItem *)p)->msgCheckBox(data, len);
}
extern "C" int msg_checkbox_screenrect(void* p, char* data, int len)
{
  return ((CheckBoxItem *)p)->msgScreenRect(data, len);
}

// messages for dropdownlist
extern "C" int msg_dropdownlist_dropdownlist(void* p, char* data, int len)
{
  return ((DropDownListItem *)p)->msgDropDownList(data, len);
}
extern "C" int msg_dropdownlist_screenrect(void* p, char* data, int len)
{
  return ((DropDownListItem *)p)->msgScreenRect(data, len);
}

//
// -----------------------
// Input
// -----------------------
//

// messages for input event handler
extern "C" int msg_iehandler_iehandler(void* p, char* data, int len)
{
  return ((InputEventHandler *)p)->msgInputEventHandler(data, len);
}


// messages for mouse
extern "C" int msg_mouse_mouse(void* p, char* data, int len)
{
  return ((Mouse *)p)->msgMouse(data, len);
}

extern "C" int msg_mouse_visible(void* p, char* data, int len)
{
  return ((Mouse *)p)->msgVisible(data, len);
}


//
// -----------------------
// Graphics3D
// -----------------------
//

// messages for g3dsystem
extern "C" int msg_g3dsystem_cmd(void* p, char* data, int len)
{
  return ((Graphics3DSystem *)p)->msgCmdGraphics3DSystem(data, len);
}

// messages for camera
extern "C" int msg_camera_frustum(void* p, char* data, int len)
{
  return ((CameraItem *)p)->msgFrustum(data, len);
}

extern "C" int msg_camera_ori(void* p, char* data, int len)
{
  return ((CameraItem *)p)->msgOri(data, len);
}

extern "C" int msg_camera_pos(void* p, char* data, int len)
{
  return ((CameraItem *)p)->msgPos(data, len);
}

// messages for light
extern "C" int msg_light_light(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgLight(data, len);
}

extern "C" int msg_light_ori(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgOri(data, len);
}

extern "C" int msg_light_pos(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgPos(data, len);
}

extern "C" int msg_light_col(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgColour(data, len);
}

// messages for geometry
extern "C" int msg_geo_geo(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgGeometry(data, len);
}

extern "C" int msg_geo_ori(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgOri(data, len);
}

extern "C" int msg_geo_pos(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgPos(data, len);
}

extern "C" int msg_geo_scale(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgScale(data, len);
}

extern "C" int msg_geo_mat(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgMaterial(data, len);
}

extern "C" int msg_geo_col(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgColour(data, len);
}



// System interface

static Graphics3DSystem* g3ds = NULL;


//
//
// ITEM CREATION
//
//


int create_item(uint64_t idItemType, char* c_initData, int len_id, void** p)
{

  //
  // Audio
  //

  if (idItemType == id_soundsource) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    SoundSourceItem *item = new SoundSourceItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_soundlistener) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    SoundListenerItem *item = new SoundListenerItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_volume) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    VolumeItem *item = new VolumeItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  //
  // GUI
  //

  if (idItemType == id_button) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    ButtonItem *item = new ButtonItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_edittext) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    EditTextItem *item = new EditTextItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_text) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    TextItem *item = new TextItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_slider) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    SliderItem *item = new SliderItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_checkbox) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    CheckBoxItem *item = new CheckBoxItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  if (idItemType == id_dropdownlist) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    DropDownListItem *item = new DropDownListItem(g3ds);
    int rv = item->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)item;
      return OK;
    } else {
      delete item;
      return rv; 
    }
  }   
 
  //
  // input
  //

  if (idItemType == id_mouse) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    Mouse *m = new Mouse(g3ds);
    int rv = m->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)m;
      return OK;
    } else {
      delete m;
      return rv; 
    }
  }   
 
  if (idItemType == id_inputeventhandler) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    InputEventHandler *ieh = new InputEventHandler(g3ds);
    int rv = ieh->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)ieh;
      return OK;
    } else {
      delete ieh;
      return rv; 
    }
  }   
 
  //
  // graphics3d
  // 

  if (idItemType == id_g3dsystem) { 
    if (g3ds) return ERROR_SYSTEM_ALREADY_INITIALIZED;
    g3ds = new Graphics3DSystem();
    int rv = g3ds->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)g3ds;
      return OK;
    } else {
      delete g3ds;
      g3ds = NULL;
      return rv; 
    }
  }   
  if (idItemType == id_camera) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    CameraItem *pc = new CameraItem(g3ds);
    int rv = pc->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)pc;
      return OK;
    } else {
      delete pc;
      return rv; 
    }
  }   
  if (idItemType == id_light) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    LightItem *pl = new LightItem(g3ds);
    int rv = pl->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)pl;
      return OK;
    } else {
      delete pl;
      return rv; 
    }
  }   
  if (idItemType == id_geo) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    GeometryItem *pg = new GeometryItem(g3ds);
    int rv = pg->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)pg;
      return OK;
    } else {
      delete pg;
      return rv; 
    }
  }   
  return ERROR_TYPE_NOT_KNOWN;
}

//
//
// ITEM DELETION
//
// an item can be destroyed by calling this function
//

int destroy_item(uint64_t idItemType, void* p)
{
  // audio

  if (idItemType == id_soundsource) {
    delete ((SoundSourceItem *) p);
    return OK;
  }   

  if (idItemType == id_soundlistener) {
    delete ((SoundListenerItem *) p);
    return OK;
  }   

  if (idItemType == id_volume) {
    delete ((VolumeItem *) p);
    return OK;
  }   

  // gui

  if (idItemType == id_button) {
    delete ((ButtonItem *) p);
    return OK;
  }   

  if (idItemType == id_edittext) {
    delete ((EditTextItem *) p);
    return OK;
  }   

  if (idItemType == id_text) {
    delete ((TextItem *) p);
    return OK;
  }   

  if (idItemType == id_slider) {
    delete ((SliderItem *) p);
    return OK;
  }   

  if (idItemType == id_checkbox) {
    delete ((CheckBoxItem *) p);
    return OK;
  }   

  if (idItemType == id_dropdownlist) {
    delete ((DropDownListItem *) p);
    return OK;
  }   

  // input

  if (idItemType == id_mouse) {
    delete ((Mouse *) p);
    return OK;
  }   

  if (idItemType == id_inputeventhandler) {
    delete ((InputEventHandler *) p);
    return OK;
  }   

  // graphics3d

  if (idItemType == id_g3dsystem) {
    delete ((Graphics3DSystem *) p);
    g3ds = NULL;
    return OK;
  }   
  if (idItemType == id_camera) {
    delete ((CameraItem *) p);
    return OK;
  }   
  if (idItemType == id_light) {
    delete ((LightItem *) p);
    return OK;
  }   
  if (idItemType == id_geo) {
    delete ((GeometryItem *) p);
    return OK;
  }   
  return ERROR_TYPE_NOT_KNOWN;
}


//
//
// MESSAGE SENDER
//
// to send other types to an item
//

int get_msg_sender(uint64_t idItemType, uint64_t idPropType, msgFP *f)
{

  // Here is the information, which type understands which message. Items which are objects 
  // can be created through the create function above. They might or might not understand messages
  // of their own type. If not after creation no change is possible, otherwise they can be set with new
  // values. 
  
  
  // audio
  
  if (idItemType == id_soundsource && idPropType == id_soundsource) {
    *f = msg_soundsource_soundsource;
    return OK;
  }   
  if (idItemType == id_soundsource && idPropType == id_playcmd) {
    *f = msg_soundsource_playcmd;
    return OK;
  }   
  if (idItemType == id_soundsource && idPropType == id_pos) {
    *f = msg_soundsource_pos;
    return OK;
  }   
  
  if (idItemType == id_soundlistener && idPropType == id_soundlistener) {
    *f = msg_soundlistener_soundlistener;
    return OK;
  }   
  if (idItemType == id_soundlistener && idPropType == id_pos) {
    *f = msg_soundlistener_pos;
    return OK;
  }   
  
  if (idItemType == id_volume && idPropType == id_volume) {
    *f = msg_volume_volume;
    return OK;
  }   
  
 
  // gui items all understand their own type and screenrect (button not)
  
  if (idItemType == id_button && idPropType == id_screenrect) {
    *f = msg_button_screenrect;
    return OK;
  }   

  if (idItemType == id_edittext && idPropType == id_edittext) {
    *f = msg_edittext_edittext;
    return OK;
  }   
    if (idItemType == id_edittext && idPropType == id_screenrect) {
    *f = msg_edittext_screenrect;
    return OK;
  }   
  
  if (idItemType == id_text && idPropType == id_text) {
    *f = msg_text_text;
    return OK;
  }   
  if (idItemType == id_text && idPropType == id_screenrect) {
    *f = msg_text_screenrect;
    return OK;
  }   
  
  if (idItemType == id_slider && idPropType == id_slider) {
    *f = msg_slider_slider;
    return OK;
  }   
  if (idItemType == id_slider && idPropType == id_screenrect) {
    *f = msg_slider_screenrect;
    return OK;
  }   
  
  if (idItemType == id_checkbox && idPropType == id_checkbox) {
    *f = msg_checkbox_checkbox;
    return OK;
  }   
  if (idItemType == id_checkbox && idPropType == id_screenrect) {
    *f = msg_checkbox_screenrect;
    return OK;
  }   
  
  if (idItemType == id_dropdownlist && idPropType == id_dropdownlist) {
    *f = msg_dropdownlist_dropdownlist;
    return OK;
  }   
  if (idItemType == id_dropdownlist && idPropType == id_screenrect) {
    *f = msg_dropdownlist_screenrect;
    return OK;
  }   
  
  // mouse and input
  
  if (idItemType == id_mouse && idPropType == id_mouse) {
    *f = msg_mouse_mouse;
    return OK;
  }   

  if (idItemType == id_mouse && idPropType == id_visible) {
    *f = msg_mouse_visible;
    return OK;
  }   

  // todo inputeventhandler needs to understand eventtypes ??
  
  
  // graphics3d
  
  // g3dsystem understands only cmd
  if (idItemType == id_g3dsystem && idPropType == id_cmdg3d) {
    *f = msg_g3dsystem_cmd;
    return OK;
  }   

  // camera understands camera, pos and ori
  if (idItemType == id_camera && idPropType == id_pos) {
    *f = msg_camera_pos;
    return OK;
  }   
  if (idItemType == id_camera && idPropType == id_ori) {
    *f = msg_camera_ori;
    return OK;
  }   

  if (idItemType == id_camera && idPropType == id_frustum) {
    *f = msg_camera_frustum;
    return OK;
  }   

  // light understands light, pos and ori, colour
  if (idItemType == id_light && idPropType == id_pos) {
    *f = msg_light_pos;
    return OK;
  }   
  if (idItemType == id_light && idPropType == id_ori) {
    *f = msg_light_ori;
    return OK;
  }   
  if (idItemType == id_light && idPropType == id_light) {
    *f = msg_light_light;
    return OK;
  }   
  if (idItemType == id_light && idPropType == id_col) {
    *f = msg_light_col;
    return OK;
  }   

  // geometry understands geo, material, colour,pos, scale and ori
  if (idItemType == id_geo && idPropType == id_pos) {
    *f = msg_geo_pos;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_ori) {
    *f = msg_geo_ori;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_scale) {
    *f = msg_geo_scale;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_geo) {
    *f = msg_geo_geo;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_mat) {
    *f = msg_geo_mat;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_col) {
    *f = msg_geo_col;
    return OK;
  }   


  return ERROR_PROPERTY_NOT_AVAILABLE;
}


//
//
// MESSAGE RECEIVER
//
//

int register_msg_receiver(uint64_t  idItemType, uint64_t idEvtType, void* p1, void* p2, msgFP2 f)
{
  if (idItemType == id_inputeventhandler && idEvtType == id_mouseevent) {
    ((InputEventHandler*)p1)->registerMouseEventFunction(f, p2, idEvtType);
    return OK;
  }
  
  if (idItemType == id_inputeventhandler && idEvtType == id_keyevent) {
    ((InputEventHandler*)p1)->registerKeyEventFunction(f, p2, idEvtType);
    return OK;
  }
 
  if (idItemType == id_edittext && idEvtType == id_edittext) {
    ((EditTextItem*)p1)->registerTextEventFunction(f, p2, idEvtType);
    return OK;
  }
  if (idItemType == id_checkbox && idEvtType == id_checkbox) {
    ((CheckBoxItem*)p1)->registerToggledEventFunction(f, p2, idEvtType);
    return OK;
  }
  if (idItemType == id_button && idEvtType == id_button) {
    printf("low level (c) register function, button item\n");
    ((ButtonItem*)p1)->registerPressedReleasedFunction(f, p2, idEvtType);
    return OK;
  }
  if (idItemType == id_slider && idEvtType == id_slider) {
    ((SliderItem*)p1)->registerSliderEventFunction(f, p2, idEvtType);
    return OK;
  }
  if (idItemType == id_dropdownlist && idEvtType == id_dropdownlist) {
    ((DropDownListItem*)p1)->registerSelectionEventFunction(f, p2, idEvtType);
    return OK;
  }
}


//
//
// ERROR MESSAGES
//
//

char *error_message(int error_id)
{
  switch (error_id) {
  case ERROR_SYSTEM_NOT_INITIALIZED : 
    return "Graphics3D: System is not initialized";
  case ERROR_SYSTEM_ALREADY_INITIALIZED : 
    return "Graphics3D: System is already initialized";
  case ERROR_PROPERTY_NOT_AVAILABLE : 
    return "Graphics3D: Item does not understand message";
  case ERROR_TYPE_NOT_KNOWN : 
    return "Graphics3D: Item type is not known (creation or deletion)";
  case ERROR_WRONG_PARAMETERS : 
    return "Graphics3D: Message send with wrong type";
  case ERROR_COULD_NOT_INITIALIZE_ENGINE :
    return "Graphics3D: Error intializing graphics engine";
  }
  return "Graphics3D: This error number is not correct";
}

