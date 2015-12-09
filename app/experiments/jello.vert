#version 330 core

uniform mat4 uModel;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition1;
uniform vec3 uRepelPosition2;
uniform float uRepelStrength;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec3 vRepel;
out     vec2 vUV;

const float r1 = .5;
const float r2 = 1.;

void main() {

    // Pass some variables to the fragment shader
    vec3 pos = vec3(uModel * vec4(aPosition, 1.0));
    vNormal   = vec3(uModel * vec4(aNormal, 0.0));

    vPosition = pos;

    vRepel = vPosition - uRepelPosition1;
    float l = length( vRepel );
    
    /*if( l < uRepelStrength ){
      vPosition -= vRepel;
    }else{
      vPosition -= normalize( vRepel ) * uRepelStrength;
    }*/

    vec3 mPos = vec3(uModel[3]);

    vec3 toSphere = mPos - uRepelPosition1;
    float dist = length( toSphere );

    float match = dot( -toSphere , vNormal );

    if( dist < r2 ){
    
        vPosition -= normalize( vRepel )  * min( l , .01 / ( l * l ));


    }
/*
    if( l < r2 && l > r1){

        vPosition -= vRepel * match * ( 1. - (l - r1 ) / (r2-r1)); //normalize( vRepel ) * r1 * ( 1. - (l - r1 ) / (r2-r1));

    }else if( l <= r1 ){

        //vPosition -= vRepel;

    } */
    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uModel * vec4(aNormal, 0.0));
    vUV = aUV;

    gl_Position = uViewProjection * vec4(vPosition, 1.0);

}