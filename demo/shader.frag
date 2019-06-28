/*
{
  "server" : 3000,
  "pixelRatio" : 1,
}
*/

precision highp float;

uniform float time;
uniform vec2 resolution;
uniform sampler2D backbuffer;
#define synth_Resolution resolution
#define veda

// begin

uniform sampler2D b0;
uniform sampler2D b1;
uniform int PASSINDEX;

#ifdef veda
#define PASSINDEX 0
#define b0 backbuffer
#endif

const float PI = 3.14159;
#define beat mod(time*140./60./2., 144.)
// #define beat max(0., time*140./60./2.-1.)
#define repeat(p,r) (mod(p,r)-r/2.)

mat2 rot (float a) { float c=cos(a), s=sin(a); return mat2(c,s,-s,c); }
float smoothmin (float a, float b, float r) { float h = clamp(.5+.5*(b-a)/r, 0., 1.); return mix(b, a, h)-r*h*(1.-h); }
float sdCylinderBox (vec2 p, vec2 r) { vec2 b = abs(p)-r; return min(0.0, max(b.x, b.y)) + length(max(b,0.0)); }
float sdTorus( vec3 p, vec2 t ) {  vec2 q = vec2(length(p.xz)-t.x,p.y); return length(q)-t.y; }
float random (in vec2 st) { return fract(sin(dot(st.xy,vec2(12.9898,78.233)))*43758.5453123); }
float hash(float n) { return fract(sin(n) * 1e4); }
float noise(vec3 x) {
    const vec3 step = vec3(110, 241, 171);
    vec3 i = floor(x);
    vec3 f = fract(x);
    float n = dot(i, step);
    vec3 u = f * f * (3.0 - 2.0 * f);
    return mix(mix(mix( hash(n + dot(step, vec3(0, 0, 0))), hash(n + dot(step, vec3(1, 0, 0))), u.x),
                   mix( hash(n + dot(step, vec3(0, 1, 0))), hash(n + dot(step, vec3(1, 1, 0))), u.x), u.y),
               mix(mix( hash(n + dot(step, vec3(0, 0, 1))), hash(n + dot(step, vec3(1, 0, 1))), u.x),
                   mix( hash(n + dot(step, vec3(0, 1, 1))), hash(n + dot(step, vec3(1, 1, 1))), u.x), u.y), u.z);
}
float fbm (vec3 p) {
  float amplitude = 0.5;
  float result = 0.0;
  for (float index = 0.0; index <= 3.0; ++index) {
    result += noise(p / amplitude) * amplitude;
    amplitude /= 2.;
  }
  return result;
}
vec3 look (vec3 eye, vec3 target, vec2 anchor) {
    vec3 forward = normalize(target-eye);
    vec3 right = normalize(cross(forward, vec3(0,1,0)));
    vec3 up = normalize(cross(right, forward));
    return normalize(forward + right * anchor.x + up * anchor.y);
}
void moda(inout vec2 p, float repetitions) {
  float angle = 2.*PI/repetitions;
  float a = atan(p.y, p.x) + angle/2.;
  a = mod(a,angle) - angle/2.;
  p = vec2(cos(a), sin(a))*length(p);
}
float sinc( float x, float k ) {
    float a = PI * (k*x-1.0);
    return sin(a)/a;
}
float sequence (float a, float b) {
  // #ifdef veda
  // float t = mod(time, 20.);
  // #else
  float t = beat;
  // #endif
  return smoothstep(b+.1,b,t) * smoothstep(a,a+.1,t);
}

float map (vec3 pos) {
  float scene = 1.0;
  float s0 = 2.0;//floor(beat);
  float b0 = 0.1;//mod(beat, 3.0)/3.;

  /*
  pos.xz *= rot(b0+s0);
  pos.yz *= rot(b0+s0);
  pos.yx *= rot(b0+s0);
  float amplitude = 1.0;
  float range = .1+.4*b0;
  float ay = .4+.1*b0+s0*.2;
  float ax = -.2-.4*b0+s0*4.;
  float az = -.5-.2*b0+s0;
  for (int index = 0; index < 6; ++index) {
    pos = abs(pos)-range*amplitude;
    pos.xz *= rot(ay*amplitude);
    pos.yz *= rot(ax*amplitude);
    pos.yx *= rot(az*amplitude);
    pos = abs(pos)-range*amplitude*.1;
    scene = min(scene, sdCylinderBox(pos.xz, vec2(.001*amplitude)));
  }
  */
  // scene = length(pos.xy)-0.05;

  const float count = 5.0;
  float amplitude = 1.0;
  float range = 2.0;
  for (float index = count; index > 0.0; --index) {
    vec3 p = pos;
    p.z = repeat(p.z, range * amplitude);
    scene = min(scene,
      max(
        max(-(length(p.xy)-0.39*amplitude), // carve inner cylinder
          max(length(p.xy)-0.4*amplitude, // outter cylinder
          abs(p.z)-0.1*amplitude)), // crop thin
        -(abs(p.z)-0.05*amplitude)) // carve thin
      );
    amplitude /= 2.0;
  }

  return scene;
}

vec3 getNormal (vec3 pos) {
  vec2 e = vec2(1.0,-1.0)*0.5773*0.00005;
  return normalize( e.xyy*map( pos + e.xyy ) + e.yyx*map( pos + e.yyx ) + e.yxy*map( pos + e.yxy ) + e.xxx*map( pos + e.xxx ) );
}

vec4 raymarch (vec3 eye, vec3 ray) {
  vec4 result = vec4(eye, 0);
  float dither = random(ray.xy+fract(time));
  float total = dither * .2;
  const float count = 50.0;
  for (float index = count; index > 0.0; --index) {
    result.xyz = eye + ray * total;
    float dist = map(result.xyz);
    if (dist < total * 1.0/synth_Resolution.y) {
      result.a = index/count;
      break;
    }
    dist *= 0.9 + 0.1 * dither;
    total += dist;
  }
  return result;
}

vec3 shade (vec3 view, vec4 pos) {
  float ao = pos.a;
  vec3 color = vec3(1.);
  vec3 normal = getNormal(pos.xyz);

  // if (stage == STAGE_INTRO) {
  //
    color = vec3(.1);
    color += vec3(0.976, 0.647, 0.482) * pow(clamp(dot(normal, normalize(vec3(.4,.4,-1.))), 0., 1.), 4.);
    color += vec3(1, 0.917, 0.760) * pow(clamp(dot(normal, normalize(vec3(-2,-4,0))), 0., 1.), 3.);
  //
  // } else if (stage == STAGE_TUNNEL) {
  //
  //     color = vec3(.1);
  //     color += vec3(0.698, 0.960, 0.909) * clamp(dot(normal, normalize(vec3(1,2,-1))), 0., 1.);
  //
  // }  else if (stage == STAGE_RING) {
  //
  //   color = vec3(0.980, 0.729, 0.478);
  //   color += vec3(0.980, 0.533, 0.478) * clamp(dot(normal, normalize(vec3(1,1,-1)))*.5+.5, 0., 1.);
  //   color += vec3(0.980, 0.078, 0.149) * pow(clamp(dot(normal, normalize(vec3(-2,-2,-4))), 0., 1.), 4.);
  //
  // } else if (stage == STAGE_CITY) {
  //
  //   vec3 light = normalize(vec3(0.,1.,0.));
  //   color += vec3(1) * abs(dot(normal, light));
  //   color += vec3(1) * pow(clamp(dot(normal, normalize(vec3(-1,-.1,-2))), 0., 1.), 2.);
  //   color += vec3(1) * pow(clamp(dot(normal, normalize(vec3(1,.1,-2))), 0., 1.), 2.);
  //
  // } else if (stage == STAGE_KIF) {
  //
  //   color = vec3(.1);
  //   color += vec3(0.760, 0.925, 1) * clamp(dot(normal, normalize(vec3(1,2,-1))), 0., 1.);
  //   color += vec3(1, 0.917, 0.760) * pow(clamp(dot(normal, normalize(vec3(-2,-4,-1))), 0., 1.), 2.);
  //   color += vec3(0.925, 1, 0.760) * pow(clamp(dot(normal, normalize(vec3(-4,0,0))), 0., 1.), 4.);
  //
  // }
  color *= ao;
  return color;
}

void main() {

  if (PASSINDEX == 0) {

    vec2 uv = (gl_FragCoord.xy-0.5*synth_Resolution)/synth_Resolution.y;
    vec3 eye = vec3(1,-2,2.);
    vec3 at = vec3(0,0,0);

    vec3 view = look(eye, at, uv);
    vec4 result = raymarch(eye, view);
    vec3 color = shade(view, result);

    vec4 frame = texture2D(b0, gl_FragCoord.xy/synth_Resolution);
    gl_FragColor = vec4(mix(frame.rgb, color, 0.9), 1);

  } else if (PASSINDEX == 1) {

    vec2 uv = gl_FragCoord.xy / synth_Resolution;
    // vec4 frame = texture2D(b1, uv);
    gl_FragColor = texture2D(b0, uv);

  }
}

//! <preset file="shader.preset" />
