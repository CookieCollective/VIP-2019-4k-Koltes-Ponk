/*
{
  "server" : 3000,
  "pixelRatio" : 1,
}
*/

precision highp float;

uniform float time;
uniform vec2 resolution, mouse;
uniform sampler2D backbuffer;
#define synth_Resolution resolution
#define veda

// begin

const float PI = 3.14159;
#define beat mod(time*140./60./2., 144.)
// #define beat max(0., time*140./60./2.-1.)
#define repeat(p,r) (mod(p,r)-r/2.)

mat2 rot (float a) { float c=cos(a), s=sin(a); return mat2(c,s,-s,c); }
float smoothmin (float a, float b, float r) { float h = clamp(.5+.5*(b-a)/r, 0., 1.); return mix(b, a, h)-r*h*(1.-h); }
float sdCylinder (vec3 pos, vec2 r) { return max(abs(pos.y)-r.y, length(pos.xz)-r.x ); }
float sdCylinderBox (vec2 p, vec2 r) { vec2 b = abs(p)-r; return min(0.0, max(b.x, b.y)) + length(max(b,0.0)); }
float sdBox (vec3 p, vec3 b) { vec3 d = abs(p) - b; return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0)); }
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
void moda(inout vec2 p, float repetitions) {
  float angle = 2.*PI/repetitions;
  float a = atan(p.y, p.x) + angle/2.;
  a = mod(a,angle) - angle/2.;
  p = vec2(cos(a), sin(a))*length(p);
}
float sequence (float t, float a, float b) {
  return smoothstep(b+.1,b,t) * smoothstep(a,a+.1,t);
}

float map (vec3 pos) {
  float scene = 1.0;
  float s0 = 2.0;//floor(beat);
  float b0 = 0.1;//mod(beat, 3.0)/3.;

  #ifdef veda
  // pos.zx *= rot(mouse.x*2.0-1.0);
  // pos.zy *= rot(mouse.y*2.0-1.0);
  #endif
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

  // float spicy = fbm(pos * 8.0 + noise(pos * 8.0) * 4.0) * 0.1 + noise(pos * 600.0) * 0.001;

  float tt = mod(time, 34.);
  float t = tt / 34.;
  const float count = 4.;
  for (float index = count; index > 0.; --index) {
    float wave = 10.*mod(time * .2 - index/count, 1.);
    vec3 p = pos/wave;
    float offset = mix(0., mod(time+index, PI*2.), smoothstep(.1,.2,t));
    p.xz *= rot(offset);
    p.yz *= rot(offset);
    p = abs(p)-.1*smoothstep(.8, 1., t);
    offset = mix(0., mod(time+index, PI*2.), smoothstep(.4,.6,t));
    p.xz *= rot(offset);
    p.yz *= rot(offset);
    scene = min(scene,wave*max(max(max(sdBox(p, vec3(.1)),-sdBox(p, vec3(.099,1,.099))),-sdBox(p, vec3(.099,.099,1))),-sdBox(p, vec3(1,.099,.099))));
    p = abs(p)-.1;
    scene = min(scene,wave*(length(p)-.01*smoothstep(.6,.8,t)));
  }

  return scene;
}

vec3 getNormal(vec3 pos){vec2 e=vec2(1.0,-1.0)*0.5773*0.0001; return normalize(e.xyy*map(pos+e.xyy)+e.yyx*map(pos+e.yyx)+e.yxy*map(pos+e.yxy)+e.xxx*map(pos+e.xxx));}

void main() {

    vec2 uv = (gl_FragCoord.xy-0.5*synth_Resolution)/synth_Resolution.y;
    vec3 eye = vec3(0,0.01,1);
    vec3 ray = normalize(vec3(uv,-1));
    vec4 result = vec4(eye, 0);
    float dither = random(ray.xy);
    float total = dither * .1;
    const float maxt = 10.0;
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

    vec3 normal = getNormal(result.xyz);
    vec3 color = vec3(.2);
    color += vec3(1.0, 0.78, 0.44) * pow(clamp(dot(normal, normalize(vec3(.4,2.,1.))), 0., 1.), 4.);
    color += vec3(1, 0.917, 0.760) * pow(clamp(dot(normal, normalize(vec3(2,-2,1))), 0., 1.), 3.);
    color *= result.a;
    color *= smoothstep(5.0, 0.0, total);

    gl_FragColor = vec4(color, 1);
}

//! <preset file="shader.preset" />
