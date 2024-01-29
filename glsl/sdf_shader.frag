
// This is mostly the standard lighting shader from Flitter, but with a simple
// SDF sphere-rendering capability hacked in. It will render the spheres
// specified by the `sphere_positions`, `sphere_radii` and `sphere_colors`
// uniforms. `smoothing` controls the use of smooth unioning of the spheres.
// `far` controls the maximum ray distance.
//
// Copyright 2024 by Jonathan Hogg and licensed under the original Flitter
// BSD 2-clause license https://github.com/jonathanhogg/flitter/blob/main/LICENSE

#version 330

const vec3 greyscale = vec3(0.299, 0.587, 0.114);

in vec3 world_position;
in vec3 world_normal;
in vec2 texture_uv;

flat in vec3 fragment_albedo;
flat in float fragment_transparency;
flat in vec3 fragment_emissive;
flat in float fragment_ior;
flat in float fragment_metal;
flat in float fragment_roughness;
flat in float fragment_occlusion;

out vec4 fragment_color;

uniform int nlights;
uniform vec4 lights[${max_lights * 4}];
uniform vec3 view_position;
uniform vec3 view_focus;
uniform bool monochrome;
uniform vec3 tint;
uniform bool orthographic;
uniform float fog_max;
uniform float fog_min;
uniform vec3 fog_color;
uniform float fog_curve;

// We need the PV matrix that is normally only used in the vertex shader
// to do per-fragment depth calculations:
uniform mat4 pv_matrix;

uniform bool use_albedo_texture;
uniform bool use_metal_texture;
uniform bool use_roughness_texture;
uniform bool use_occlusion_texture;
uniform bool use_emissive_texture;
uniform bool use_transparency_texture;

uniform sampler2D albedo_texture;
uniform sampler2D metal_texture;
uniform sampler2D roughness_texture;
uniform sampler2D occlusion_texture;
uniform sampler2D emissive_texture;
uniform sampler2D transparency_texture;


// These should be provided as attributes to the render `!group`:
const int NSPHERES = ${NSPHERES};
uniform vec3 sphere_positions[NSPHERES];
uniform vec3 sphere_colors[NSPHERES];
uniform float sphere_radii[NSPHERES];
uniform float far;
uniform float smoothing = 0;
uniform int max_iterations = 250;
uniform float normal_delta = 1;
uniform float epsilon = 1;


// Basic ray-marching SDF implementation:

const float INFINITY = 1.0 / 0.0;
const vec3 CONSTANTS = vec3(1.0, 0.0, -1.0);

vec3 NORMAL_DX = vec3(normal_delta, 0.0, 0.0);
vec3 NORMAL_DY = vec3(0.0, normal_delta, 0.0);
vec3 NORMAL_DZ = vec3(0.0, 0.0, normal_delta);

struct Trace {
    float d;
    vec3 c;
    vec3 n;
};

vec4 union_sdf(vec4 d1, vec4 d2) {
    return d1.a <= d2.a ? d1 : d2;
}

vec4 smooth_union_sdf(vec4 d1, vec4 d2, float k) {
    float h = clamp(0.5 + 0.5 * (d2.a - d1.a)/k, 0.0, 1.0), d = k * h*(1.0-h);
    return mix(d2, d1, h) + CONSTANTS.yyyz*d;
}

vec4 sphere_sdf(vec3 point, vec3 origin, float radius, vec3 color) {
    return vec4(color, length(point - origin) - radius);
}

vec4 scene_sdf(vec3 point) {
    vec4 d = vec4(vec3(0.0), INFINITY);
    for (int i=0; i < NSPHERES; i++) {
        vec4 sd = sphere_sdf(point, sphere_positions[i], sphere_radii[i], sphere_colors[i]);
        d = smoothing > 0.0 ? smooth_union_sdf(d, sd, smoothing) : union_sdf(d, sd);
    }
    return d;
}

Trace ray_march(vec3 origin, vec3 direction, float max_distance) {
    vec4 dist;
    vec3 p;
    float d = 0;
    for (int i = 0; i < max_iterations; i++) {
        p = origin + d * direction;
        dist = scene_sdf(p);
        float delta = dist.a;
        if (abs(delta) < epsilon) {
            break;
        }
        d += delta;
        if (d > max_distance) {
            return Trace(INFINITY, vec3(0), vec3(0));
        }
    }
    vec3 d1 = vec3(scene_sdf(p + NORMAL_DX).a, scene_sdf(p + NORMAL_DY).a, scene_sdf(p + NORMAL_DZ).a),
         d2 = vec3(scene_sdf(p - NORMAL_DX).a, scene_sdf(p - NORMAL_DY).a, scene_sdf(p - NORMAL_DZ).a);
    return Trace(d, dist.rgb, normalize(d1 - d2));
}


void main() {
    vec3 V;
    float view_distance;
    if (orthographic) {
        V = normalize(view_position - view_focus);
        view_distance = dot(view_position - world_position, V);
    } else {
        V = view_position - world_position;
        view_distance = length(V);
        V /= view_distance;
    }

    // This is the additional SDF bit.
    //
    // We ray-march through the fragment in the view direction to get the
    // distance, emissive colour, and normal for this fragment. The fragment
    // depth is overridden to match the actual ray intersection point instead
    // of the surface of the render volume.
    //
    Trace trace = ray_march(world_position, -V, far-view_distance);
    if (trace.d < 0 || isinf(trace.d)) {
        discard;
    }
    vec3 emissive = trace.c;
    vec3 N = trace.n;
    vec4 pos = pv_matrix * vec4(world_position - V*trace.d, 1);
    gl_FragDepth = pos.z / pos.w;
    view_distance += trace.d;
    //
    // The rest of the material properties and lighting calculation is done
    // as per usual.

    float fog_alpha = (fog_max > fog_min) && (fog_curve > 0) ? pow(clamp((view_distance - fog_min) / (fog_max - fog_min), 0, 1), 1/fog_curve) : 0;
    if (fog_alpha == 1) {
        discard;
    }
    vec3 albedo = fragment_albedo;
    if (use_albedo_texture) {
        vec4 texture_color = texture(albedo_texture, texture_uv);
        albedo = albedo * (1 - clamp(texture_color.a, 0, 1)) + texture_color.rgb;
    }
    float metal = fragment_metal;
    if (use_metal_texture) {
        vec4 texture_color = texture(metal_texture, texture_uv);
        float mono = clamp(dot(texture_color.rgb, greyscale), 0, 1);
        metal = metal * (1 - clamp(texture_color.a, 0, 1)) + mono;
    }
    float roughness = fragment_roughness;
    if (use_roughness_texture) {
        vec4 texture_color = texture(roughness_texture, texture_uv);
        float mono = clamp(dot(texture_color.rgb, greyscale), 0, 1);
        roughness = roughness * (1 - clamp(texture_color.a, 0, 1)) + mono;
    }
    float occlusion = fragment_occlusion ;
    if (use_occlusion_texture) {
        vec4 texture_color = texture(occlusion_texture, texture_uv);
        float mono = clamp(dot(texture_color.rgb, greyscale), 0, 1);
        occlusion = occlusion * (1 - clamp(texture_color.a, 0, 1)) + mono;
    }
    // vec3 emissive = fragment_emissive;
    if (use_emissive_texture) {
        vec4 emissive_texture_color = texture(emissive_texture, texture_uv);
        emissive = emissive * (1 - clamp(emissive_texture_color.a, 0, 1)) + emissive_texture_color.rgb;
    }
    float transparency = fragment_transparency;
    if (use_transparency_texture) {
        vec4 texture_color = texture(transparency_texture, texture_uv);
        float mono = clamp(dot(texture_color.rgb, greyscale), 0, 1);
        transparency = transparency * (1 - clamp(texture_color.a, 0, 1)) + mono;
    }
    vec3 diffuse_color = vec3(0);
    vec3 specular_color = emissive;
    // vec3 N = normalize(world_normal);
    float rf0 = (fragment_ior - 1) / (fragment_ior + 1);
    vec3 F0 = mix(vec3(rf0*rf0), albedo, metal);
    for (int i = 0; i < nlights * 4; i += 4) {
        int light_type = int(lights[i].w);
        float inner_cone = lights[i+1].w;
        float outer_cone = lights[i+2].w;
        vec3 light_color = lights[i].xyz;
        vec3 light_position = lights[i+1].xyz;
        vec3 light_direction = lights[i+2].xyz;
        vec4 light_falloff = lights[i+3];
        if (light_type == ${Ambient}) {
            diffuse_color += (1 - F0) * (1 - metal) * albedo * light_color * occlusion;
        } else {
            vec3 L;
            float attenuation = 1;
            float light_distance = 1;
            if (light_type == ${Point}) {
                L = light_position - world_position;
                light_distance = length(L);
                L /= light_distance;
            } else if (light_type == ${Spot}) {
                L = light_position - world_position;
                light_distance = length(L);
                L /= light_distance;
                float spot_cosine = dot(L, -light_direction);
                attenuation = 1 - clamp((inner_cone-spot_cosine) / (inner_cone-outer_cone), 0, 1);
            } else {
                L = -light_direction;
            }
            float ld2 = light_distance * light_distance;
            vec4 ds = vec4(1, light_distance, ld2, light_distance * ld2);
            attenuation /= dot(ds, light_falloff);
            vec3 H = normalize(V + L);
            float NdotL = clamp(dot(N, L), 0, 1);
            float NdotV = clamp(dot(N, V), 0, 1);
            float NdotH = clamp(dot(N, H), 0, 1);
            float HdotV = clamp(dot(H, V), 0, 1);
            float a = roughness * roughness;
            float a2 = a * a;
            float denom = NdotH * NdotH * (a2-1) + 1;
            float NDF = a2 / (denom * denom);
            float r = roughness + 1;
            float k = (r*r) / 8;
            float G = (NdotV / (NdotV * (1 - k) + k)) * (NdotL / (NdotL * (1 - k) + k));
            vec3 F = F0 + (1 - F0) * pow(1 - HdotV, 5);
            vec3 diffuse = (1 - F) * (1 - metal) * albedo;
            vec3 specular = (NDF * G * F) / (4 * NdotV * NdotL + 1e-6);
            vec3 radiance = light_color * attenuation * NdotL;
            diffuse_color += diffuse * radiance;
            specular_color += specular * radiance;
        }
    }
    float opacity = 1 - transparency;
    vec3 final_color = mix(diffuse_color, fog_color, fog_alpha) * opacity + specular_color * (1 - fog_alpha);
    if (monochrome) {
        float grey = dot(final_color, greyscale);
        final_color = vec3(grey);
    }
    fragment_color = vec4(final_color * tint, opacity);
}
