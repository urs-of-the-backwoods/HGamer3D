// Simple Lighting Shader for HGamer3D
// does ADS shading, plus bump-maps plus normal maps
// running on Ogre engine, version 1.8.1, OpenGL

// Fragment Program

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

// Uniforms
///////////

uniform vec4 ambient;
uniform sampler2D u_diffuseTexture;

#if defined(LIGHTING)

#if defined(BUMPED)
uniform sampler2D u_normalmapTexture;
#endif

#if defined(SPECMAP)
uniform sampler2D u_specularTexture;
#endif 

uniform vec4 u_lightPositions[LIGHT_COUNT];
uniform vec4 u_lightDiffuseColours[LIGHT_COUNT];

#if !defined(BUMPED)
uniform vec4 u_lightDirections[LIGHT_COUNT];   // if BUMPED light directions taken from position 
#endif

#if defined(SPECULAR)
uniform float u_specularExponent;
uniform vec4 u_lightSpecularColours[LIGHT_COUNT];
#endif

uniform vec4 u_lightAttenuations[LIGHT_COUNT];   // Ogre, 4 parameters per light: range, constant attenuation, linear att., quadratic att.

#if defined(SPOTLIGHT)
uniform vec4 u_spotlightParameters[LIGHT_COUNT];   // Ogre, 4 parameters per light, cos (inner/2), cos (outer/2), falloff, 1 (=0 if not a spotlight)
#endif

#endif   // LIGHTING 

// Variables
////////////

vec4 _baseColor;

// Varyings
///////////

varying vec2 v_texCoord;

#if defined(LIGHTING)

#if !defined(BUMPED)
varying vec3 v_normalVector;
#endif

varying vec3 v_lightDirections[LIGHT_COUNT];

#if defined(SPOTLIGHT)
varying vec3 v_spotlightDirections[LIGHT_COUNT];
#endif

#if defined(SPECULAR)
varying vec3 v_cameraDirection; 
#endif

#endif  // LIGHTING


vec3 computeLighting(vec3 normalVector, vec3 lightDirection, vec3 lightColor, float attenuation)
{
    // used external, global variables: u_specularExponent, _baseColor.rgb

    float diffuse = max(dot(normalVector, lightDirection), 0.0);
    vec3 diffuseColor = lightColor * _baseColor.rgb * diffuse * attenuation;

    #if defined(SPECULAR)

    // Phong shading
    // vec3 vertexToEye = normalize(v_cameraDirection);
    // vec3 specularAngle = normalize(normalVector * diffuse * 2.0 - lightDirection);  
    // vec3 specularColor = vec3(pow(clamp(dot(specularAngle, vertexToEye), 0.0, 1.0), u_specularExponent)); 

    // Blinn-Phong shading
    vec3 vertexToEye = normalize(v_cameraDirection);
    vec3 halfVector = normalize(lightDirection + vertexToEye);
    float specularAngle = clamp(dot(normalVector, halfVector), 0.0, 1.0);
    vec3 specularColor = vec3(pow(specularAngle, u_specularExponent)) * attenuation;

    return diffuseColor * 0.5 + specularColor * 0.5 ;

    #else
    
    return diffuseColor;
    
    #endif
}

vec3 getLitPixel()
{
    #if defined(BUMPED)
    
    vec3 normalVector = normalize(texture2D(u_normalmapTexture, v_texCoord).rgb * 2.0 - 1.0);
//    vec3 normalVector = normalize(texture2D(u_normalmapTexture, v_texCoord).rgb );
    
    #else
    
    vec3 normalVector = normalize(v_normalVector);
    
    #endif
 
    // here was ambient light somewhere DEBUG - replace 0.0 !   
    vec3 combinedColor = _baseColor.rgb * 0.1;

    // Directional light contribution
    float attenuation = 1.0;
    for (int i = 0; i < LIGHT_COUNT; ++i)
    {
        #if defined(BUMPED)
        vec3 lightDirection = normalize(v_lightDirections[i] * 2.0);
        #else
        vec3 lightDirection = normalize(u_lightDirections[i].xyz * 2.0);  // there are no u_lightDirections currently !!!
        #endif 

	// Attenuation calculations point lights and spot light due to range
	if (u_lightPositions[i].w > 0.5) {	// point light or spot light, no directional light
	   vec3 ldir = v_lightDirections[i] * (1.0 / u_lightAttenuations[i].x);
	   attenuation = clamp(1.0 - dot(ldir, ldir), 0.0, 1.0);
	}

	// Attenuation spotlight due to spot
	#if defined(SPOTLIGHT)
	if (u_spotlightParameters[i].w > 0.5) {	// this is a spotlight
           	// "-lightDirection" is used because light direction points in opposite direction to spot direction.
	        float spotCurrentAngleCos = dot(lightDirection, -v_spotlightDirections[i]);
		float innerCos = u_spotlightParameters[i].x;
		float outerCos = u_spotlightParameters[i].y;

		// Apply spot attenuation
		attenuation *= smoothstep(outerCos, innerCos, spotCurrentAngleCos);
	}
	#endif

	attenuation = 1.0;  // DEBUG
        combinedColor += computeLighting(normalVector, lightDirection, u_lightDiffuseColours[i].xyz, attenuation);
    }

    return combinedColor;
}


void main()
{
    _baseColor = texture2D(u_diffuseTexture, v_texCoord);
 
    gl_FragColor.a = _baseColor.a;

    #if defined(TEXTURE_DISCARD_ALPHA)
    if (gl_FragColor.a < 0.5)
        discard;
    #endif

    #if defined(LIGHTING)
    gl_FragColor.rgb = getLitPixel();
    #else
    gl_FragColor.rgb = _baseColor.rgb;
    #endif
}
