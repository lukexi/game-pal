#version 330 core

uniform vec3 uCamera;
uniform vec4 uDiffuse;
uniform float uTime;

in      vec3 vPos;
in      vec3 vNorm;
in      vec3 vRepel;
in      vec2 vUv;

out     vec4 fragColor;

const   vec3 lightColor = vec3(1);
const   float ambient = 0.2;

void main() {
    vec3 lightPosition = uCamera;
    
    //calculate normal in world coordinates
    vec3 normal = normalize(vNorm);

    //calculate the location of this fragment in world coordinates
    vec3 surfacePos = vPos;
    
    // vec4 surfaceColor = texture(materialTex, fragTexCoord);
    vec4 surfaceColor = uDiffuse;
    vec3 surfaceToLight = normalize(lightPosition - surfacePos);

    // Calculate final color of the pixel, based on:
    // 1. The angle of incidence: diffuseCoefficient
    // 2. The color/intensities of the light: lightColor
    // 3. The diffuse color: surfaceColor

    float diffuseCoefficient = max(ambient, dot(normal, surfaceToLight));
    vec3 diffuseLit = diffuseCoefficient * surfaceColor.rgb * lightColor;


    fragColor = vec4( 1. , 1. , 1. , 1. );

   // fragColor = vec4( normalize( vRepel ) / length( vRepel ) , 1. );
    fragColor = vec4( vNorm * .5 + .5 , 1. );// * sin( uTime * 6.28);

    //fragColor = vec4( sin( vUv.x) , 0.  , 0. , 1.);
}
