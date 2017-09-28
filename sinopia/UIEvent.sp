# position and element data, which is relevant during a UI event
struct UIEventPosition {
	# position of mouse on the screen
	mousePosition : Position2D;
	# position of element involved
	elementPosition : Position2D;	
	# name of element involved, if available, if not empty string
	elementName : Text;
}

# a UI mouse click event
enum UIClick {
	# mouse click down, parameters are involved position data and mouse data
	Down UIEventPosition MouseButtonData;
	# mouse click up, parameters are involved position data and mouse data
	Up UIEventPosition MouseButtonData;
}

id64 UIClick = 0x07669c3c292bf265

# a UI mouse hover event
enum UIHover {
	# hover, parameters are involved position data and mouse data
	Hover UIEventPosition MouseButtonData;
}

id64 UIHover = 0x7e6f5eb9ae275c30

# ui mouse drag event
enum UIDrag {
	# begin drag event, parameters are involved position data and mouse data
	Begin UIEventPosition MouseButtonData;
	# move drag event, parameters are involved position data and mouse data
	Move UIEventPosition MouseButtonData;
	# end drag event, parameters are involved position data and mouse data
	End UIEventPosition MouseButtonData;
}

id64 UIDrag = 0x239d21291230fb3a
