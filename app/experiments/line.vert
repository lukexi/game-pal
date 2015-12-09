#version 330 core

uniform mat4 uModel;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition1;
uniform float uRepelStrength;

uniform vec3 uStartPoint;
uniform vec3 uEndPoint;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;
out     float vLength;



vec3 cubicCurve( float t , vec3  c0 , vec3 c1 , vec3 c2 , vec3 c3 ){
  
  float s  = 1. - t; 

  vec3 v1 = c0 * ( s * s * s );
  vec3 v2 = 3. * c1 * ( s * s ) * t;
  vec3 v3 = 3. * c2 * s * ( t * t );
  vec3 v4 = c3 * ( t * t * t );

  vec3 value = v1 + v2 + v3 + v4;

  return value;

}

void main() {

    float val = aPosition.x;

    vec3 endP       = uEndPoint;
    vec3 startP     = uStartPoint;

    vec3 dir = endP - startP;

    vLength = length( dir );

    vec3 endPDown = uEndPoint - vec3( .2 , 0. , 0. ) - .1 * dir;
    vec3 startPUp = uStartPoint + vec3( .2 , 0. , 0. )+ .1 * dir;




    vec3 p0 = vec3(0.);
    vec3 v0 = vec3(0.);
    vec3 p1 = vec3(0.);
    vec3 v1 = vec3(0.);

    vec3 p2 = vec3(0.);


    float base = val * 3.;
    float baseUp   = floor( base );
    float baseDown = ceil( base );
    float amount = base - baseUp;

    if( baseUp == 0. ){

        p0 = startP;
        p1 = startPUp;
        p2 = endPDown;


        v1 = .5 * ( p2 - p0 );

    }else if( baseDown == 3. ){

        p0 = endPDown;
        p1 = endP;
        p2 = startPUp;

        v0 = .5 * ( p1 - p2 );

    }else if( baseUp == 1. ){

        p0 = startPUp;
        p1 = endPDown;


        vec3 pMinus;

        pMinus = startP;
        p2 = endP;

        v1 = .5 * ( p2 - p0 );
        v0 = .5 * ( p1 - pMinus );

    }


    vec3 c0 = p0;
    vec3 c1 = p0 + v0/3.;
    vec3 c2 = p1 - v1/3.;
    vec3 c3 = p1;






    vec3 pos = cubicCurve( amount , c0 , c1 , c2 , c3 );


    //vec3 dir = uEndPoint - uStartPoint;

    //vec3 pos = uStartPoint + dir * aPosition.x;
    // Pass some variables to the fragment shader
    //vec3 pos = vec3(uModel * vec4(aPosition, 1.0));

    vPosition = pos;

    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uModel * vec4(aNormal, 0.0));
    vUV = aUV;

    gl_Position = uViewProjection * vec4(vPosition, 1.0);

}