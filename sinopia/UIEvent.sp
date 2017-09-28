# a UI mouse click event, sent by UI Subsystem, not by a single element
enum UIClickEvent {

	# no click, for initializing
	NoClick;

	# mouse single click, parameters: element name, mouse position, button data
	SingleClick Text Position2D MouseButtonData;

	# mouse double click, parameters: element name, mouse position, button data
	DoubleClick Text Position2D MouseButtonData;

	# mouse click end, parameters: element name, mouse position, button data
	ClickEnd Text Position2D MouseButtonData;
}

id64 UIClickEvent = 0x07669c3c292bf265

# a UI mouse hover event, sent by a single element
enum UIHoverEvent {

	# no hover, for initializing
	NoHover;

	# hover begin, parameters: element name, element position, mouse position
	HoverBegin Text Position2D Position2D;

	# hover end, parameters: element name
	HoverEnd Text;
}

id64 UIHoverEvent = 0x7e6f5eb9ae275c30

# ui mouse drag event
enum UIDragEvent {

	# no drag event, for initializing
	NoDrag;

	# begin drag event, parameters: element name, element position, mouse position
	DragBegin Text Position2D Position2D;

	# move drag event, parameters: element name, element position, delta position, mouse position
	DragMove Text Position2D Position2D Position2D;

	# end drag event, parameters: element name, element position, mouse position
	DragEnd Text Position2D Position2D;

	# cancel drag event, cancelled by escape key, parameters: element name, element position, mouse position
	DragCancel Text Position2D Position2D;
}

id64 UIDragEvent = 0x239d21291230fb3a
