enum FaceCameraMode
{
  FCNone;
  FCRotateXYZ;
  FCRotateY;
  FCLookatXYZ;
  FCLookatY;
  FCLookatMixed;
  FCDirection;
}

struct Text3D
{
  Font : Text;
  FontSize : Int32;
  FCMode : FaceCameraMode;
  FixedScreenSize : Bool;
}

id64 Text3D = 0x620bb5dd7dfca052
