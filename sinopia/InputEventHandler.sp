enum InputEventType {
    IEMouseButtonUp;
    IEMouseButtonDown;
    IEMouseMove;
    IEMouseButtonWheel;
    IEMouseVisible;
    IEKeyUp;
    IEKeyDown;
    IEExitRequested;
}

enum InputEventHandler {
    DefaultEventHandler;
    SpecificEventHandler List(InputEventType);
}

id64 InputEventHandler = 0xfc0edefcebcb5878
