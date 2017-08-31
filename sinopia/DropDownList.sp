enum TextSelection {
    Selection Int32;
    NoSelection;
}

struct DropDownList {
    content : List(Text);
    selected : TextSelection;
}

id64 DropDownList = 0x200de0e837a8e590
