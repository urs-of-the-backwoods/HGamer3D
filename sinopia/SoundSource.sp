enum SoundType {
    Sound;
    Sound3D;
    Music;
}

struct SoundSource {
    type : SoundType;
    resource : Text;
    loop : Bool;
    volume : Float32;
    volumeGroup : Text;
} 

id64 SoundSource = 0xafcef7aa41d88c0d
