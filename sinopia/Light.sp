import Angle 0

enum LightType {
    PointLight;
    DirectionalLight;
    SpotLight Angle Float32;   # Angle: field of view, Float: aspect ratio
}

struct Light {
    type : LightType;
    brightness : Float32;
    range : Float32;
    specularIntensity : Float32;
}

id64 Light = 0x981e80e50d994ea9