import ScreenRect 0x16877957e32da6b1
import Angle 0

enum Camera {
    FullViewCamera;
    OverlayCamera ScreenRect Float32;
}

id64 Camera = 0xd3b0d455ab1f4716

struct Frustum {
    nearDistance : Float32;
    farDistance : Float32;
    fieldOfViewHorizontal : Angle;
}

id64 Frustum = 0xf3ce3235d4f8e73d
