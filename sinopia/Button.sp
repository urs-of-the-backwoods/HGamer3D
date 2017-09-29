# Button, containing text and event bool (to be deprecated)
struct Button {
    pressed : Bool;
    label : Text;
}

id64 Button = 0x68a1857c27690b30


# Button event, signals press and release actions
enum ButtonEvent {
	NoButtonEvent;
	Pressed;
	Released;
}

id64 ButtonEvent = 0x3049202a50c414a7


# Standard UI Button with ordinary style
type BasicButton = Text

id64 BasicButton = 0x372110c1ae4548b8


# UI Button, based on an image, which can be clicked to signal button action
type ImageButton = Text

id64 ImageButton = 0x916f39acd0fc989e
