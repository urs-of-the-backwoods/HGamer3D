struct KeyData {
    key : Int32;
    scancode : Int32;
    name : Text;
}

enum KeyEvent {
    NoKeyEvent;
    KeyUpEvent KeyData;
    KeyDownEvent KeyData;
}

id64 KeyEvent = 0x5ba1617fb50e97e5
