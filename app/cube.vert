#version 330 core

// TODO:
// Make it so that push "unstretches"
// as you get further away from edge. 
// aka no hard jumps when you get too far away!

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

const float bufferDistance = 1.;
const float distanceCutoff = .5;
const float maxDepth = 1.;

float distanceToPlane( vec3 n , vec3 p1 , vec3 p2 , out vec3 perp , out vec3 para ){
  
  vec3 dif = p2 - p1;

  float d = dot( n , dif );

  perp = n * d;

  para = dif - perp;

  return d;

}

float getDisplacement( vec3 norm , vec3 pos ){

  vec3 perp;
  vec3 para;

  float d = distanceToPlane( vNormal , pos , uRepelPosition , perp , para );

  if( d > 0. || d < -maxDepth){ d = 0.; }

  d = -d * ( -maxDepth - d);

  float len = length( para );

  float push = 0.;

  if( len <= distanceCutoff ){
    push = pow( ( distanceCutoff - len ) / distanceCutoff , 3. );
  }

  float finalPush = max( -bufferDistance , d * push );

  return finalPush;

}

vec3 getNormal( vec3 normal , vec3 position , vec3 tangent ){

  vec3 mNorm = (uModel * vec4( normal , 0. )).xyz;
  mNorm = normalize( mNorm );

  vec3 binormal = cross( normal , tangent );
  float distance = .1;

  vec3 pUpX = position + distance * tangent;//vec3(   distance ,  0. , 0. );
  vec3 pDoX = position - distance * tangent;//vec3(  -distance ,  0. , 0. );
  vec3 pUpY = position + distance * binormal;//vec3(   .0 ,  distance , 0. );
  vec3 pDoY = position - distance * binormal; //vec3(   .0 , -distance , 0. );


  vec3 mUpX = (uModel * vec4( pUpX , 1. )).xyz;
  vec3 mDoX = (uModel * vec4( pDoX , 1. )).xyz;
  vec3 mUpY = (uModel * vec4( pUpY , 1. )).xyz;
  vec3 mDoY = (uModel * vec4( pDoY , 1. )).xyz;

  float dUpX = getDisplacement( mNorm , mUpX );
  float dDoX = getDisplacement( mNorm , mDoX );
  float dUpY = getDisplacement( mNorm , mUpX );
  float dDoY = getDisplacement( mNorm , mDoY );

  vec3 fUpX = pUpX + normal * dUpX * 10.;
  vec3 fDoX = pDoX + normal * dDoX * 10.;
  vec3 fUpY = pUpY + normal * dUpY * 10.;
  vec3 fDoY = pDoY + normal * dDoY * 10.;

  vec3 difX = fUpX - fDoX;
  vec3 difY = fUpY - fDoY;

  vec3 norm = cross( difX , difY );
  norm = normalize( norm );

  return norm;

}


void main() {

    // Pass some variables to the fragment shader
    vec3 pos = vec3(uModel * vec4(aPosition, 1.0));

    

    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uModel * vec4(aNormal, 0.0));

    float push = getDisplacement( vNormal , pos );
    
    vPosition = pos + push * vNormal;

    vNormal = vec3(uModel * vec4(getNormal( aNormal , aPosition , aTangent ),0.));
    gl_Position = uViewProjection * vec4(vPosition, 1.0);

}