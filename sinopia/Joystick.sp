struct Joystick {
  index : Int32;     // index of joystick, starting at 0 (first one)
}

id64 Joystick = 0xe5ea0d693a04ff71

enum JoystickEvent {
  NoJoystickEvent;
  ButtonDown Int32;        // Button Id
  ButtonUp Int32;          // Button Id
  AxisMove Int32 Float32;  // Axis Id, Move Position
  HatMove Int32 Int32;     // Axis Id, Hat Position
}

id64 JoystickEvent = 0x1cdc5b0a65479346

enum JoystickButton {
  A;
  B;
  Back;
  DPadDown;
  DPadLeft;
  DPadRight;
  DPadUp;
  Guide;
  LeftShoulder;
  LeftStick;
  RightShoulder;
  RightStick;
  Start;
  X;
  Y;
}

enum JoystickAxis {
  LeftX;
  LeftY;
  RightX;
  RightY;
  TriggerLeft;
  TriggerRight;
}
