// Simple Lighting Shader for HGamer3D
// does ADS shading, plus bump-maps plus normal maps
// running on Ogre engine, version 1.8.1, OpenGL

// Vertex Program

// DEFINES
//////////

// possible defines
// LIGHT_COUNT needs to be defined
// BUMPED normalmap is there
// SPECULAR use specular lighting
// SPECMAP specularmap is there
// LIGHT_COUNT is number of lights which must be present, can be more (nth nearest are used)
// SPOTLIGHT is defined, if spotlights are used at all

#ifndef LIGHT_COUNT
#define LIGHT_COUNT=0
#endif

#if LIGHT_COUNT > 0
#define LIGHTING
#endif

// Attributes
/////////////

attribute vec4 position;
attribute vec2 uv0;

#if defined(LIGHTING)
attribute vec3 normal;

#if defined(BUMPED)
attribute vec3 tangent;
attribute vec3 binormal;
#endif

#endif


// Uniforms
///////////

uniform mat4 u_worldViewProjectionMatrix;

#if defined(LIGHTING)
uniform mat4 u_inverseTransposeWorldMatrix;
uniform mat4 u_worldMatrix;

uniform vec4 u_lightPositions[LIGHT_COUNT];

#if defined(SPOTLIGHT) 
uniform vec4 u_spotlightDirections[LIGHT_COUNT];
#endif

#if defined(SPECULAR)
uniform vec3 u_cameraPosition;
#endif

#endif // LIGHTING


// Varyings

varying vec2 v_texCoord;

#if defined(LIGHTING)

#if !defined(BUMPED)
varying vec3 v_normalVector;
#endif

varying vec3 v_lightDirections[LIGHT_COUNT];  // light direction in tangent space, for spotlights from position

#if defined(SPOTLIGHT)
varying vec3 v_spotlightDirections[LIGHT_COUNT];  // used for attenuation of spotlights only
#endif

#if defined(SPECULAR)
varying vec3 v_cameraDirection;
#endif

#endif



// FUNCTIONS
// /////////


#if defined(BUMPED)
void applyLight(vec4 position, mat3 tangentSpaceTransformMatrix)
{
    vec4 positionWorldSpace = u_worldMatrix * position;

    // transform light directions to tangent space

    for (int i = 0; i < LIGHT_COUNT; ++i)
    {
	if (u_lightPositions[i].w > 0.5) {	// directional light w == 0 else 0
		v_lightDirections[i] = tangentSpaceTransformMatrix * (u_lightPositions[i].xyz - positionWorldSpace.xyz);
	} else {
		v_lightDirections[i] = tangentSpaceTransformMatrix * (- u_lightPositions[i].xyz);
	}
	
	#if defined(SPOTLIGHT)
	v_spotlightDirections[i] = tangentSpaceTransformMatrix * u_spotlightDirections[i];
	#endif

    }

    #if defined(SPECULAR)
    // Compute camera direction and transform it to tangent space.
    v_cameraDirection = tangentSpaceTransformMatrix * (u_cameraPosition - positionWorldSpace.xyz);
    #endif
}
#else
void applyLight(vec4 position)
{
    vec4 positionWorldSpace = u_worldMatrix * position;
    
    for (int i = 0; i < LIGHT_COUNT; ++i)
    {
	if (u_lightPositions[i].w > 0.5) {	// directional light w == 0 else 1
		v_lightDirections[i] = (u_lightPositions[i].xyz - positionWorldSpace.xyz);
	} else {
		v_lightDirections[i] = (- u_lightPositions[i].xyz);
	}
    }

    #if defined(SPECULAR)  
    v_cameraDirection = u_cameraPosition - positionWorldSpace.xyz;
    #endif
}

#endif


void main()
{
    gl_Position = u_worldViewProjectionMatrix * position;

    #if defined(LIGHTING)
    // Transform the normal, tangent and binormals to view space. (not correct, somehow), we need the inversTransposeWorld matrix!
    mat3 inverseTransposeWorldMatrix = mat3(u_inverseTransposeWorldMatrix[0].xyz, u_inverseTransposeWorldMatrix[1].xyz, u_inverseTransposeWorldMatrix[2].xyz);
    vec3 normalVector = normalize(inverseTransposeWorldMatrix * normal);
    
    #if defined(BUMPED)
    
    vec3 tangentVector  = normalize(inverseTransposeWorldMatrix * tangent);
    vec3 binormalVector = normalize(inverseTransposeWorldMatrix * binormal);
    mat3 tangentSpaceTransformMatrix = mat3(tangentVector.x, binormalVector.x, normalVector.x, tangentVector.y, binormalVector.y, normalVector.y, tangentVector.z, binormalVector.z, normalVector.z);
    applyLight(position, tangentSpaceTransformMatrix);
    
    #else
    
    v_normalVector = normalVector;
    applyLight(position);
    
    #endif
    
    #endif // LIGHTING
    
    v_texCoord = uv0;
   
}
