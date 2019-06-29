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

  // float spicy = fbm(pos * 8.0 + noise(pos * 8.0) * 4.0) * 0.1 + noise(pos * 600.0) * 0.001;
  // float tt = mod(time, 130.);
  float tt = mod(time, 144.);
  // tt = mod(tt, 34.);
  // float tt = 34.+mod(time, 41.);
  // float tt = 75.+mod(time,35.);
  // float tt = mod(time, 130.);
  // float tt = 130.+mod(time, 14.);
  if (tt < 34.) {
    float t = tt / 34.;
    const float count = 4.;
    for (float index = count; index > 0.; --index) {
      float wave = 10.*mod(tt * .3 - index/count, 1.);
      vec3 p = pos/wave;
      float offset = mix(0., mod(tt+index, PI*2.), smoothstep(.1,.15,t));
      p.xz *= rot(offset);
      p.yz *= rot(offset);
      p = abs(p)-.1*smoothstep(.8, 1., t);
      offset = mix(0., mod(tt+index, PI*2.), smoothstep(.4,.45,t));
      p.xz *= rot(offset);
      p.yz *= rot(offset);
      scene = min(scene,wave*max(max(max(sdBox(p, vec3(.1)),-sdBox(p, vec3(.095,1,.095))),-sdBox(p, vec3(.095,.095,1))),-sdBox(p, vec3(1,.095,.095))));
      p = abs(p)-.1;
      scene = min(scene,wave*(length(p)-.04*smoothstep(.6,.8,t)+.03));
    }
  } else if (tt < 75.) {
    float t = (tt-34.) / 41.;
    const float count = 6.;
    float amplitude = 1.0;
    float t1 = smoothstep(.0,1.,t);
    pos.xz *= rot(mix(0., mod(t*5.98, PI*2.), t1));
    pos.yz *= rot(mix(0., mod(t*5.64, PI*2.), t1));
    pos.yx *= rot(mix(0., mod(t*5.35, PI*2.), t1));
    for (float index = count; index > 0.; --index) {
      vec3 p = pos;
      float yy = floor((p.z-tt*amplitude)/amplitude);
      p.z = repeat(p.z-tt*amplitude, amplitude);
      float offset = mix(0., (t+index+mod(yy,PI*2.)), smoothstep(.0,.5,t));
      p.xz *= rot(offset);
      p.yz *= rot(offset);
      p = p.xzy;
      scene = smoothmin(scene, max(-(sdCylinder(p, vec2(.5,1.)*amplitude)), sdCylinder(p, vec2(.51,.02)*amplitude)), 0.2*amplitude);
      amplitude /= 1.1;
    }
    pos.xy *= rot(-tt);
    pos = abs(pos);
    moda(pos.xy, 1.+3.*smoothstep(.5,1.,t));
    pos.x -= .1+.05*sin(pos.z * 8. + tt * 2.);
    float zz = floor(pos.z/.2);
    pos.z = repeat(pos.z, .2);
    // scene = smoothmin(scene, length(pos)+.01-.03*smoothstep(.4,.5,t), .1);
    pos.xz *= rot(tt+zz);
    pos.yz *= rot(tt+zz);
    float t2 = smoothstep(.3,.5,t);
    scene = smoothmin(scene, sdBox(pos, vec3(-.1+.2*t2)), .1*t2);
    float r1 = .1*t2;
    float th1 = .09*t2;
    // scene = smoothmin(scene, max(max(max(sdBox(pos, vec3(r1)),-sdBox(pos, vec3(th1,1,th1))),-sdBox(pos, vec3(th1,th1,1))),-sdBox(pos, vec3(1,th1,th1))), .1*t2);
  } else if (tt < 130.) {
    float t = (tt-75.) / 55.;
    // float t1 = smoothstep(.5,.8,t);
    vec3 pp = pos;
    pos.xz *= rot(tt*.098);
    pos.yz *= rot(tt*.064);
    pos.yx *= rot(tt*.035);
    pos.z += tt;
    vec3 p = pos;
    // const float count = 5.;
    float amplitude = 1.;
    for (float index = 5.; index > 0.; --index) {
      // p = abs(p)-1.*amplitude;
      // p.yz *= rot(.2);
      // p.yx *= rot(.2);
      p = repeat(p, mix(4.5,3.0,t)*amplitude);
      // p.xy *= rot(rt);
      scene = min(scene, abs(sdBox(p, vec3(amplitude)))-.5*amplitude);
      // scene = smoothmin(scene, abs(sdBox(p, vec3(amplitude)))-.5*amplitude, .01*amplitude);
      // scene = smoothmin(scene, abs(length(p)-amplitude)-.9*amplitude, .1*amplitude);
      amplitude /= 3.;
    }
    scene = max(-1., -scene);
    scene = max(scene, -(length(pos.xy)-1.));

    pos = pp;
    float s0 = floor(tt);
    float b0 = fract(tt);
    pos.xz *= rot(b0+s0);
    pos.yz *= rot(b0+s0);
    pos.yx *= rot(b0+s0);
    amplitude = 1.0;
    float range = .05*sin(b0*PI);
    float ay = .4+.1*b0+s0*.2;
    float ax = -.2-.4*b0+s0*4.;
    float az = -.5-.2*b0+s0;
    for (int index = 0; index < 4; ++index) {
      pos = abs(pos)-range*amplitude;
      pos.xz *= rot(ay*amplitude);
      pos.yz *= rot(ax*amplitude);
      pos.yx *= rot(az*amplitude);
      pos = abs(pos)-range*amplitude*.1;
      scene = smoothmin(scene, sdCylinderBox(pos.xz, vec2(.001*amplitude)), .05*amplitude);
    }

  }

  return scene;
}

vec3 getNormal(vec3 pos){vec2 e=vec2(1.0,-1.0)*0.5773*0.0001; return normalize(e.xyy*map(pos+e.xyy)+e.yyx*map(pos+e.yyx)+e.yxy*map(pos+e.yxy)+e.xxx*map(pos+e.xxx));}

void main() {

    vec2 uv = (gl_FragCoord.xy-0.5*synth_Resolution)/synth_Resolution.y;
    vec3 eye = vec3(0,0.01,1);
    vec3 ray = normalize(vec3(uv,-.9));
    vec4 result = vec4(eye, 0);
    float dither = random(ray.xy);
    float total = dither * .4;
    const float maxt = 10.0;
    const float count = 50.0;
    for (float index = count; index > 0.0; --index) {
      result.xyz = eye + ray * total;
      float dist = map(result.xyz);
      // if (dist < total * 1./synth_Resolution.y) {
      if (dist < total * 1./synth_Resolution.y) {
        // result.a += .02;
      // }
      // if (result.a >= 1.) {
        result.a = index/count;
        break;
      }
      // dist = max(.01,dist);
      dist *= 0.9 + 0.1 * dither;
      total += dist;
    }

    vec3 normal = getNormal(result.xyz);
    vec3 color = vec3(1.);
    // color += vec3(0.44, 0.65, 1.0) * pow(clamp(dot(normal, normalize(vec3(.4,2.,1.))), 0., 1.), 4.);
    // color += vec3(0.96, 0.79, 0.2) * pow(clamp(dot(normal, normalize(vec3(2,-2,1))), 0., 1.), 3.);
    color += pow(clamp(dot(normal, normalize(vec3(.4,2.,1.))), 0., 1.), 4.);
    color += pow(clamp(dot(normal, normalize(vec3(2,-2,1))), 0., 1.), 3.);
    color *= result.a;
    // float lod = 8.0;
    // color = ceil(color/lod);
    // color *= smoothstep(5.0, 4.0, total);
    // color = smoothstep(.0,.8, color);

    ray.xz *= rot(time*.2);
    ray.yz *= rot(time*.1);
    // vec3 bg = 1.0-(ray*0.5+0.5);
    // float should = step(0.0001,(color.r+color.b+color.g)/3.);
    float should = smoothstep(0.0, .8,(color.r+color.b+color.g)/3.);
    uv.y -= sqrt(abs(uv.x)+.01)*.35-.1;
    float heart = smoothstep(0.0, 0.001, length(uv)-1.5*smoothstep(130., 144., mod(time, 144.)));
    vec3 bg = mix(vec3(0.92, 0.12, 0.12), vec3(1), smoothstep(.4,.5,sin(ray.y*100.)));
    bg = mix(vec3(1), bg, heart);
    color = mix(bg,color, should*smoothstep(mod(time,144.)<34.?1.:10., 0.0, length(result.xyz)));
    // color = mix(1.-bg, bg, heart);

    gl_FragColor = vec4(color, 1);
}

//! <preset file="shader.preset" />
