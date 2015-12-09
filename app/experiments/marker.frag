#version 330 core

uniform vec3 uCamera;


in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;


out     vec4 fragColor;

void main() {

  
    fragColor = vec4(1. ,1. ,1. , 1.);

   // fragColor = vec4( normalize( vRepel ) / length( vRepel ) , 1. );
    fragColor = vec4( vNormal * .5 + .5 , 1. );

    fragColor = vec4( vUV.x , 0. , .5 , 1. );
}