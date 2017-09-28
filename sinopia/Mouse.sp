enum MouseMode {
    Absolute;
    Relative;
    Wrap;
}

struct MouseConfig {
    mode : MouseMode;
}

id64 MouseConfig = 0xa532f43b1c1c6bc7

struct MouseButtonData {
    button : Int32;
    buttons : Int32;
    qualifiers : Int32;
}

struct MouseMoveData {
    x : Int32;  
    y : Int32; 
    dx : Int32;
    dy : Int32;
    buttons : Int32;
    qualifiers : Int32;
}

struct MouseWheelData {
    wheel : Int32;
    buttons : Int32;
    qualifiers : Int32;
}

enum MouseEvent {
    NoMouseEvent;
    MButtonUpEvent MouseButtonData;
    MButtonDownEvent MouseButtonData;
    MMoveEvent MouseMoveData;
    MWheelEvent MouseWheelData;
}

id64 MouseEvent = 0x27eaf3fd46595d08

enum MouseClickEvent {
    NoMouseClick;
    MouseDownClick MouseButtonData;
    MouseUpClick MouseButtonData;
}

id64 MouseClickEvent = 0x5bd46a46b4ae5d38

enum MouseMoveEvent {
    NoMouseMove;
    MouseMove MouseMoveData;
}

id64 MouseMoveEvent = 0x7a409f478c6a34f5

enum MouseWheelEvent {
    NoMouseWheel;
    MouseWheel MouseWheelData;
}

id64 MouseWheelEvent = 0xa5d6c1c359e6d8ce
