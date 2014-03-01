/*
  Basic ambient lighting vertex program
*/
void ambientOneTexture_vp(float4 position : POSITION,
						  float2 uv		  : TEXCOORD0,
						  
						  out float4 oPosition : POSITION,
						  out float2 oUv	   : TEXCOORD0,
						  out float4 colour    : COLOR,

						  uniform float4x4 worldViewProj,
						  uniform float4 ambient)
{
	oPosition = mul(worldViewProj, position);
	oUv = uv;
	colour = ambient;
}

