# direction in which sub elements are laid out
enum LayoutMode {
	# no automatic layout
	LMFree;

	# elements are ordered horizontally
	LMHorizontal;

	# elements are ordered vertically
	LMVertical;
}

# how elements are placed on the screen
struct Layout {
	# direction of layout or free
	mode : LayoutMode;

	# space between elements
	spacing : Int32;

	# borders of element
	borders : ScreenRect2;
}

id64 Layout = 0x1c94738af20a0ac6