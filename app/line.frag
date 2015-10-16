#version 330 core

uniform vec3 uCamera;
uniform vec4 uDiffuse;
uniform float uTime;

in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;
in      float vLength;

out     vec4 fragColor;

void main() {
   // fragColor = vec4( sin( vUV.x * vLength * 20. - uTime  * vLength ) );
    fragColor = vec4( 1. );
}