
// This is mostly the standard lighting shader from Flitter, but with a simple
// SDF sphere-rendering capability hacked in and texture-mapping support
// removed. It will render the spheres specified by the `sphere_positions`,
// `sphere_radii` and `sphere_colors` uniforms. `smoothing` controls the use of
// smooth unioning of the spheres. `far` controls the maximum ray distance.
//
// Copyright 2024 by Jonathan Hogg and licensed under the original Flitter
// BSD 2-clause license https://github.com/jonathanhogg/flitter/blob/main/LICENSE

#version 330

const vec3 greyscale = vec3(0.299, 0.587, 0.114);

in vec3 world_position;
in vec3 world_normal;
in vec2 texture_uv;
in vec2 coord;

flat in vec4 fragment_albedo;
flat in vec3 fragment_emissive;
flat in vec4 fragment_properties;

out vec4 fragment_color;

uniform int nlights;
uniform lights_data {
    vec4 lights[${max_lights * 4}];
};
uniform vec3 view_position;
uniform vec3 view_focus;
uniform bool monochrome;
uniform vec3 tint;
uniform bool orthographic;
uniform float fog_max;
uniform float fog_min;
uniform vec3 fog_color;
uniform float fog_curve;

// These should be provided as attributes to the render `!group`. Note that
// these values can be compiled into the code as constants if they are unlikely
// to change or passed in as uniforms otherwise.
//
const int NSPHERES = ${NSPHERES};
const int far = ${far};
const float smoothing = ${smoothing};
const int max_iterations = ${max_iterations};
const float normal_delta = ${normal_delta};
const float epsilon = ${epsilon};
uniform vec3 sphere_positions[NSPHERES];
uniform vec3 sphere_colors[NSPHERES];
uniform float sphere_radii[NSPHERES];
uniform sampler2D depth_buffer;
uniform mat4 pv_matrix;

// Basic ray-marching SDF implementation:

const float INFINITY = 1.0 / 0.0;
vec2 NORMAL = vec2(normal_delta, 0);

struct Trace {
    float d;
    vec3 c;
    vec3 n;
};

// We have a signed distance function that does only distance calculations...
//
float scene_sdf(vec3 point) {
    float d = length(point - sphere_positions[0]) - sphere_radii[0];
    for (int i = 1; i < NSPHERES; i++) {
        float sd = length(point - sphere_positions[i]) - sphere_radii[i];
        if (smoothing > 0) {
            float h = clamp(0.5 + 0.5*(sd-d)/smoothing, 0, 1);
            d = mix(sd, d, h) - smoothing*h*(1-h);
        } else if (sd < d) {
            d = sd;
        }
    }
    return d;
}

// ...and one that also calculates the color.
//
vec4 scene_sdf_rgb(vec3 point) {
    float d = length(point - sphere_positions[0]) - sphere_radii[0];
    vec3 rgb = sphere_colors[0];
    for (int i = 1; i < NSPHERES; i++) {
        float sd = length(point - sphere_positions[i]) - sphere_radii[i];
        if (smoothing > 0) {
            float h = clamp(0.5 + 0.5*(sd-d)/smoothing, 0, 1);
            d = mix(sd, d, h) - smoothing*h*(1-h);
            rgb = mix(sphere_colors[i], rgb, h);
        } else if (sd < d) {
            d = sd;
            rgb = sphere_colors[i];
        }
    }
    return vec4(rgb, d);
}

// Our ray marcher does standard spherical marching using the distance-only
// function first and then does one more iteration with the color function
// and 6 more calls to the distance function to calculate a surface normal.
//
Trace ray_march(vec3 origin, vec3 direction, float max_distance) {
    vec3 p = origin;
    float d = 0;
    for (int i = 1; i < max_iterations; i++) {
        float delta = scene_sdf(p);
        d += delta;
        if (d > max_distance) {
            return Trace(INFINITY, vec3(0), vec3(0));
        }
        p = origin + d * direction;
        if (abs(delta) < epsilon) {
            break;
        }
    }
    vec4 rgbd = scene_sdf_rgb(p);
    d += rgbd.a;
    p = origin + d * direction;
    vec3 d1 = vec3(scene_sdf(p + NORMAL.xyy), scene_sdf(p + NORMAL.yxy), scene_sdf(p + NORMAL.yyx)),
         d2 = vec3(scene_sdf(p - NORMAL.xyy), scene_sdf(p - NORMAL.yxy), scene_sdf(p - NORMAL.yyx));
    return Trace(d, rgbd.rgb, normalize(d1 - d2));
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
    // We ray-march backwards from the fragment in the view direction to find
    // an intersection with the SDF scene. If there is no hit then we exit
    // immediately with transparent as the fragment colour. Otherwise we use
    // the normal lighting calculation with the world position, normal and
    // emissive colour derived from the SDF functions. Note that we are still
    // colouring the surface fragment of the render triangular mesh and using
    // its Z for the depth buffer. Effectively we treat the rendered volumes
    // as pocket universes that contain the SDF scene.

    // Do a test against our pre-calculated depth buffer to see if this is an
    // overdrawn fragment that can be ignored:
    vec4 depth = texture(depth_buffer, coord);
    if (depth.a == 1 && view_distance > depth.r + 0.25) {
        fragment_color = vec4(0);
        return;
    }

    // Ray-march to find the actual position, normal and emissive colour of this
    // fragment:
    Trace trace = ray_march(world_position, -V, far-view_distance);
    if (isinf(trace.d)) {
        fragment_color = vec4(0);
        return;
    }
    vec3 emissive = trace.c;
    vec3 N = trace.n;
    view_distance += trace.d;
    vec3 world_pos = world_position - V*trace.d;

    // The rest of the material properties and lighting calculation is done
    // as per usual.

    float fog_alpha = (fog_max > fog_min) && (fog_curve > 0) ? pow(clamp((view_distance - fog_min) / (fog_max - fog_min), 0, 1), 1/fog_curve) : 0;
    if (fog_alpha == 1) {
        fragment_color = vec4(0);
        return;
    }
    vec3 albedo = fragment_albedo.rgb;
    float transparency = fragment_albedo.a;
    // vec3 emissive = fragment_emissive;
    float ior = fragment_properties.x;
    float metal = fragment_properties.y;
    float roughness = fragment_properties.z;
    float occlusion = fragment_properties.z;

    vec3 diffuse_color = vec3(0);
    vec3 specular_color = emissive;
    // vec3 N = normalize(world_normal);
    float rf0 = (ior - 1) / (ior + 1);
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
                L = light_position - world_pos; // was world_position
                light_distance = length(L);
                L /= light_distance;
            } else if (light_type == ${Spot}) {
                L = light_position - world_pos; // was world_position
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
    vec4 pos = pv_matrix * vec4(world_pos, 1);
}
