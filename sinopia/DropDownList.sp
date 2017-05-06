enum MaybeInt {
    Just Int32;
    Nothing;
}

struct DropDownList {
    content : List(Text);
    selected : MaybeInt;
}

id64 DropDownList = 0x200de0e837a8e590
