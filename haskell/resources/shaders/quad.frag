#version 330 core
out vec4 FragColor;

in vec2 TexCoord;

uniform sampler2D inTexture;
uniform uint colorMode; // 0 = flat color, 1 = color * texture, 2 = texture
uniform vec4 color;
uniform vec2 inResolution;
uniform float startFade;
uniform float endFade;
uniform bool doFXAA;

// rb3 venue postprocessing
uniform bool doPostProcess;
uniform uint postProcessStart;
uniform float postProcessFraction; // 0 (start) to 1 (end)
uniform uint postProcessEnd;

/**
GLSL FXAA implementation from https://github.com/mattdesl/glsl-fxaa/

Basic FXAA implementation based on the code on geeks3d.com with the
modification that the texture2DLod stuff was removed since it's
unsupported by WebGL.
--
From:
https://github.com/mitsuhiko/webgl-meincraft
Copyright (c) 2011 by Armin Ronacher.
Some rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * The names of the contributors may not be used to endorse or
      promote products derived from this software without specific
      prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef FXAA_REDUCE_MIN
  #define FXAA_REDUCE_MIN   (1.0/ 128.0)
#endif
#ifndef FXAA_REDUCE_MUL
  #define FXAA_REDUCE_MUL   (1.0 / 8.0)
#endif
#ifndef FXAA_SPAN_MAX
  #define FXAA_SPAN_MAX     8.0
#endif

//optimized version for mobile, where dependent
//texture reads can be a bottleneck
vec4 fxaa(sampler2D tex, vec2 fragCoord, vec2 resolution,
      vec2 v_rgbNW, vec2 v_rgbNE,
      vec2 v_rgbSW, vec2 v_rgbSE,
      vec2 v_rgbM) {
  vec4 color;
  mediump vec2 inverseVP = vec2(1.0 / resolution.x, 1.0 / resolution.y);
  vec3 rgbNW = texture(tex, v_rgbNW).xyz;
  vec3 rgbNE = texture(tex, v_rgbNE).xyz;
  vec3 rgbSW = texture(tex, v_rgbSW).xyz;
  vec3 rgbSE = texture(tex, v_rgbSE).xyz;
  vec4 texColor = texture(tex, v_rgbM);
  vec3 rgbM  = texColor.xyz;
  vec3 luma = vec3(0.299, 0.587, 0.114);
  float lumaNW = dot(rgbNW, luma);
  float lumaNE = dot(rgbNE, luma);
  float lumaSW = dot(rgbSW, luma);
  float lumaSE = dot(rgbSE, luma);
  float lumaM  = dot(rgbM,  luma);
  float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
  float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));

  mediump vec2 dir;
  dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
  dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));

  float dirReduce = max((lumaNW + lumaNE + lumaSW + lumaSE) *
                        (0.25 * FXAA_REDUCE_MUL), FXAA_REDUCE_MIN);

  float rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);
  dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX),
            max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX),
            dir * rcpDirMin)) * inverseVP;

  vec3 rgbA = 0.5 * (
    texture(tex, fragCoord * inverseVP + dir * (1.0 / 3.0 - 0.5)).xyz +
    texture(tex, fragCoord * inverseVP + dir * (2.0 / 3.0 - 0.5)).xyz);
  vec3 rgbB = rgbA * 0.5 + 0.25 * (
    texture(tex, fragCoord * inverseVP + dir * -0.5).xyz +
    texture(tex, fragCoord * inverseVP + dir * 0.5).xyz);

  float lumaB = dot(rgbB, luma);
  if ((lumaB < lumaMin) || (lumaB > lumaMax))
    color = vec4(rgbA, texColor.a);
  else
    color = vec4(rgbB, texColor.a);
  return color;
}

void texcoords(vec2 fragCoord, vec2 resolution,
    out vec2 v_rgbNW, out vec2 v_rgbNE,
    out vec2 v_rgbSW, out vec2 v_rgbSE,
    out vec2 v_rgbM) {
  vec2 inverseVP = 1.0 / resolution.xy;
  v_rgbNW = (fragCoord + vec2(-1.0, -1.0)) * inverseVP;
  v_rgbNE = (fragCoord + vec2(1.0, -1.0)) * inverseVP;
  v_rgbSW = (fragCoord + vec2(-1.0, 1.0)) * inverseVP;
  v_rgbSE = (fragCoord + vec2(1.0, 1.0)) * inverseVP;
  v_rgbM = vec2(fragCoord * inverseVP);
}

vec4 apply(sampler2D tex, vec2 fragCoord, vec2 resolution) {
  mediump vec2 v_rgbNW;
  mediump vec2 v_rgbNE;
  mediump vec2 v_rgbSW;
  mediump vec2 v_rgbSE;
  mediump vec2 v_rgbM;

  //compute the texture coords
  texcoords(fragCoord, resolution, v_rgbNW, v_rgbNE, v_rgbSW, v_rgbSE, v_rgbM);

  //compute FXAA
  return fxaa(tex, fragCoord, resolution, v_rgbNW, v_rgbNE, v_rgbSW, v_rgbSE, v_rgbM);
}

// Claude Code came up with most of these
vec3 applyPostProcess(vec3 color, uint effectIndex) {
  if (effectIndex == 0u) {
    // V3_ProFilm_a - film look with slight warmth
    color = pow(color, vec3(0.9));
    color = mix(color, color * vec3(1.1, 1.0, 0.9), 0.3);
    return color;
  } else if (effectIndex == 1u) {
    // V3_ProFilm_b - cooler film look
    color = pow(color, vec3(0.85));
    color = mix(color, color * vec3(0.9, 1.0, 1.1), 0.3);
    return color;
  } else if (effectIndex == 2u) {
    // V3_video_a - video look with slight saturation boost
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    return mix(vec3(luma), color, 1.2);
  } else if (effectIndex == 3u) {
    // V3_film_16mm - grainy film look
    float noise = fract(sin(dot(TexCoord, vec2(12.9898, 78.233))) * 43758.5453);
    color = mix(color, color + (noise - 0.5) * 0.1, 0.5);
    return pow(color, vec3(0.9));
  } else if (effectIndex == 4u) {
    // V3_shitty_tv - old TV/security camera look
    float scanline = sin(TexCoord.y * 800.0) * 0.04;
    color = mix(color, vec3(dot(color, vec3(0.333))), 0.3);
    return color + scanline;
  } else if (effectIndex == 5u) {
    // V3_bloom - bright bloom effect
    return color * 1.5 + pow(color, vec3(0.5)) * 0.3;
  } else if (effectIndex == 6u) {
    // V3_film_sepia_ink - sepia tone
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    return vec3(luma * 1.2, luma * 1.0, luma * 0.8);
  } else if (effectIndex == 7u) {
    // V3_film_silvertone - silver/monochrome with blue tint
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    return vec3(luma * 0.9, luma * 0.95, luma * 1.1);
  } else if (effectIndex == 8u) {
    // V3_film_b_w - black and white
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    return vec3(luma);
  } else if (effectIndex == 9u) {
    // V3_video_bw - video black and white
    float luma = dot(color, vec3(0.333));
    return vec3(luma);
  } else if (effectIndex == 10u) {
    // V3_contrast_a - high contrast
    return (color - 0.5) * 1.5 + 0.5;
  } else if (effectIndex == 11u) {
    // V3_photocopy - high contrast black and white
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    luma = (luma - 0.5) * 3.0 + 0.5;
    return vec3(clamp(luma, 0.0, 1.0));
  } else if (effectIndex == 12u) {
    // V3_film_blue_filter - blue filter
    return color * vec3(0.7, 0.8, 1.3);
  } else if (effectIndex == 13u) {
    // V3_desat_blue - desaturated with blue tint
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    vec3 desat = mix(vec3(luma), color, 0.3);
    return desat * vec3(0.9, 0.95, 1.2);
  } else if (effectIndex == 14u) {
    // V3_video_security - security camera look
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    vec3 greenTint = vec3(luma * 0.8, luma * 1.2, luma * 0.8);
    float scanline = sin(TexCoord.y * 600.0) * 0.03;
    return greenTint + scanline;
  } else if (effectIndex == 15u) {
    // V3_bright - brightened
    return color * 2.5;
  } else if (effectIndex == 16u) {
    // V3_posterize - posterization effect
    return floor(color * 6.0) / 6.0;
  } else if (effectIndex == 17u) {
    // V3_clean_trails - subtle trails/blur
    return mix(color, color * 0.9, 0.1);
  } else if (effectIndex == 18u) {
    // V3_video_trails - video trails
    return mix(color, color * 0.8, 0.2);
  } else if (effectIndex == 19u) {
    // V3_flicker_trails - flickering trails
    float flicker = sin(TexCoord.x * 50.0 + TexCoord.y * 30.0) * 0.1 + 0.9;
    return color * flicker;
  } else if (effectIndex == 20u) {
    // V3_desat_posterize_trails - desaturated posterized with trails
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    vec3 desat = mix(vec3(luma), color, 0.4);
    return floor(desat * 5.0) / 5.0;
  } else if (effectIndex == 21u) {
    // V3_film_contrast - film with high contrast
    color = pow(color, vec3(0.8));
    return (color - 0.5) * 1.3 + 0.5;
  } else if (effectIndex == 22u) {
    // V3_film_contrast_blue - film contrast with blue tint
    color = pow(color, vec3(0.8));
    color = (color - 0.5) * 1.3 + 0.5;
    return color * vec3(0.8, 0.9, 1.3);
  } else if (effectIndex == 23u) {
    // V3_film_contrast_green - film contrast with green tint
    color = pow(color, vec3(0.8));
    color = (color - 0.5) * 1.3 + 0.5;
    return color * vec3(0.8, 1.3, 0.9);
  } else if (effectIndex == 24u) {
    // V3_film_contrast_red - film contrast with red tint
    color = pow(color, vec3(0.8));
    color = (color - 0.5) * 1.3 + 0.5;
    return color * vec3(1.3, 0.8, 0.9);
  } else if (effectIndex == 25u) {
    // V3_horror_movie_special - black (light input) on bright red (dark input)
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    return mix(vec3(1.0, 0.0, 0.0), vec3(0.0), luma);
  } else if (effectIndex == 26u) {
    // V3_photo_negative - photo negative
    return 1.0 - color;
  } else if (effectIndex == 27u) {
    // V3_ProFilm_mirror_a - mirror effect (basic implementation, special blend handled elsewhere)
    return color;
  } else if (effectIndex == 28u) {
    // V3_ProFilm_psychedelic_blue_red - bright red (light input) on bright blue (dark input)
    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    return mix(vec3(0.0, 0.0, 1.0), vec3(1.0, 0.0, 0.0), luma);
  } else if (effectIndex == 29u) {
    // V3_space_woosh - space effect
    float dist = distance(TexCoord, vec2(0.5));
    float woosh = 1.0 - smoothstep(0.0, 0.7, dist);
    return color * (1.0 + woosh * 0.5) + vec3(woosh * 0.2);
  }

  // Default case - return original color
  return color;
}

// Special mirror effect function
vec3 applyMirrorEffect(vec3 startColor, vec3 endColor, float fraction) {
  vec2 center = vec2(0.5, 0.5);
  vec2 pos = TexCoord - center;
  
  // Determine mirror strength based on which effect is the mirror
  float mirrorStrength;
  bool startIsMirror = (postProcessStart == 27u);
  bool endIsMirror = (postProcessEnd == 27u);
  
  if (startIsMirror && endIsMirror) {
    // Both are mirror - full mirror always
    mirrorStrength = 1.0;
  } else if (startIsMirror) {
    // Transitioning away from mirror: high fraction = less mirror
    mirrorStrength = 1.0 - fraction;
  } else {
    // Transitioning to mirror: high fraction = more mirror
    mirrorStrength = fraction;
  }
  
  // Check if we should mirror this pixel
  bool shouldMirror = false;
  vec2 mirrorCoord = TexCoord;
  
  if (mirrorStrength < 2.0/3.0) {
    // Phase 1: Rotating radius line from center
    // 0.0 = pointing right, 1/3 = pointing up, 2/3 = pointing left
    float lineAngle = mirrorStrength * 3.0 * 3.14159 * 0.5; // 0 to 3π/2
    
    // Get pixel angle from center
    float pixelAngle = atan(pos.y, pos.x);
    if (pixelAngle < 0.0) pixelAngle += 2.0 * 3.14159; // 0 to 2π
    
    // Check if pixel is in swept area (counter-clockwise from right)
    bool inSweptArea = false;
    if (lineAngle <= 3.14159) {
      // Normal case: swept area from 0 to lineAngle
      inSweptArea = (pixelAngle >= 0.0 && pixelAngle <= lineAngle);
    } else {
      // Wrapped case: from 0 to lineAngle, handling wraparound
      // MT: not sure if this is necessary?
      float wrappedAngle = lineAngle - 2.0 * 3.14159;
      inSweptArea = (pixelAngle >= 0.0 && pixelAngle <= lineAngle) ||
                    (pixelAngle >= wrappedAngle && pixelAngle <= 2.0 * 3.14159);
    }
    
    if (inSweptArea) {
      shouldMirror = true;
      // Fix reflection: reflect across line through center
      vec2 lineDir = vec2(cos(lineAngle), sin(lineAngle));
      vec2 normal = vec2(-lineDir.y, lineDir.x); // Perpendicular to line
      float d = dot(pos, normal);
      vec2 reflected = pos - 2.0 * d * normal;
      mirrorCoord = center + reflected;
    }
  } else {
    // Phase 2: Full line mirroring (2/3 to 1.0)
    // At 2/3: horizontal line (top half mirrors bottom half)
    // At 1.0: vertical line (left half mirrors right half)
    float t = (mirrorStrength - 2.0/3.0) / (1.0/3.0); // 0 to 1
    float lineAngle = t * 3.14159 * 0.5; // 0 to π/2 (0° to 90°)
    
    // Check which side of the line we're on
    vec2 lineDir = vec2(cos(lineAngle), sin(lineAngle));
    vec2 normal = vec2(-lineDir.y, lineDir.x);
    float d = dot(pos, normal);
    
    if (d > 0.0) { // On one side of the line
      shouldMirror = true;
      // Reflect across the line
      vec2 reflected = pos - 2.0 * d * normal;
      mirrorCoord = center + reflected;
    }
  }
  
  if (shouldMirror) {
    // Apply mirroring - mirrored pixels are 100% replaced
    mirrorCoord = clamp(mirrorCoord, vec2(0.0), vec2(1.0));
    vec3 mirroredColor = texture(inTexture, mirrorCoord).rgb;

    if (startIsMirror) {
      // Start is mirror, so use mirrored pixel with start effect
      // Blend this mirrored result with the end effect
      vec3 mirroredStart = applyPostProcess(mirroredColor, postProcessStart);
      vec3 normalEnd = applyPostProcess(mirroredColor, postProcessEnd);
      return mix(mirroredStart, normalEnd, fraction);
    } else {
      // End is mirror, so use mirrored pixel with end effect
      // Blend start effect with this mirrored result
      vec3 normalStart = applyPostProcess(mirroredColor, postProcessStart);
      vec3 mirroredEnd = applyPostProcess(mirroredColor, postProcessEnd);
      return mix(normalStart, mirroredEnd, fraction);
    }
  } else {
    // No mirroring, normal blend
    return mix(startColor, endColor, fraction);
  }
}

void main()
{
  if (colorMode == 0u) {
    FragColor = color;
    return;
  } else if (colorMode == 1u) {
    FragColor = color * texture(inTexture, TexCoord);
    return;
  }

  vec4 result
    = doFXAA
    ? apply(inTexture, TexCoord * inResolution, inResolution)
    : texture(inTexture, TexCoord)
    ;

  if (doPostProcess) {
    vec3 startEffect = applyPostProcess(result.rgb, postProcessStart);
    vec3 endEffect = applyPostProcess(result.rgb, postProcessEnd);
    
    // Special handling for mirror effect
    if (postProcessStart == 27u || postProcessEnd == 27u) {
      result.rgb = applyMirrorEffect(startEffect, endEffect, postProcessFraction);
    } else {
      result.rgb = mix(startEffect, endEffect, postProcessFraction);
    }
  }

  // note highway horizon fade
  if (endFade > startFade) {
    float horizonFade = 1.0 - (TexCoord.y - startFade) / (endFade - startFade);
    if (horizonFade > 1.0) horizonFade = 1.0;
    if (horizonFade < 0.0) horizonFade = 0.0;
    result = vec4(result.rgb, result.a * horizonFade);
  }

  FragColor = result;
}
