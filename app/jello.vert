#version 330 core

uniform mat4 uModel;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition;
uniform float uRepelStrength;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec3 vRepel;

void main() {

    // Pass some variables to the fragment shader
    vec3 pos = vec3(uModel * vec4(aPosition, 1.0));

    vPosition = pos;

    vRepel = vPosition - uRepelPosition;
    float l = length( vRepel );
    
    if( l < uRepelStrength ){
      vPosition -= vRepel;
    }else{
      vPosition -= normalize( vRepel ) * uRepelStrength;
    }
    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uModel * vec4(aNormal, 0.0));

    gl_Position = uViewProjection * vec4(vPosition, 1.0);

}