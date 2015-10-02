#version 330 core

uniform vec3 uCamera;


in      vec3 vPosition;
in      vec3 vNormal;


out     vec4 fragColor;

void main() {

  
    fragColor = vec4(1. ,1. ,1. , 1.);

   // fragColor = vec4( normalize( vRepel ) / length( vRepel ) , 1. );
    //fragColor = vec4( vNormal * .5 + .5 , 1. ) * sin( uTime * 6.28);
}