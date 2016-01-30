namespace HGamer3D.Graphics3D.Light

enum LightType {
    pointLight;
    directionalLight;
    spotLight Angle Float32;
}

struct Light {
    lt : LightType;
    brightness : Float32;
    range : Float32;
    specularIntensity : Float32;    
}

id64 Light = 0x981e80e50d994ea9

