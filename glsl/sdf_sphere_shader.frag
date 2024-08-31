${HEADER}

// This is mostly the standard lighting shader from Flitter, but with a simple
// SDF sphere-rendering capability hacked in and texture-mapping support
// removed. It will render the spheres specified by the `sphere_positions`,
// `sphere_radii` and `sphere_colors` uniforms. `smoothing` controls the use of
// smooth unioning of the spheres.
//
// Copyright 2024 by Jonathan Hogg and licensed under the original Flitter
// BSD 2-clause license https://github.com/jonathanhogg/flitter/blob/main/LICENSE

in vec3 world_position;

flat in vec4 fragment_albedo;
flat in vec4 fragment_properties;

out vec4 fragment_color;

uniform vec3 view_position;
uniform vec3 view_focus;
uniform bool monochrome;
uniform vec3 tint;
uniform bool orthographic;
uniform float fog_max;
uniform float fog_min;
uniform vec3 fog_color;
uniform float fog_curve;
uniform mat4 pv_matrix;

// These should be provided as attributes to the render `!group`. Note that
// these values can be compiled into the code as constants if they are unlikely
// to change or passed in as uniforms otherwise.
//
const int NSPHERES = ${NSPHERES};
const float smoothing = ${float(smoothing)};
const int max_iterations = ${int(max_iterations)};
const float epsilon = ${float(epsilon)};
uniform vec3 sphere_positions[NSPHERES];
uniform vec3 sphere_colors[NSPHERES];
uniform float sphere_radii[NSPHERES];

// Use PBR lighting calculations from Flitter core:
//
<%include file="color_functions.glsl"/>
<%include file="lighting_functions.glsl"/>

// Basic ray-marching SDF implementation:

int start_sphere, end_sphere;
bool sphere_include[NSPHERES];

const float INFINITY = 1.0 / 0.0;
const vec2 NORMAL_DELTA = vec2(${float(normal_delta)}, 0.0);

struct Trace {
    float d;
    vec3 c;
    vec3 n;
    int i;
};

// We have a signed distance function that does only distance calculations...
//
float scene_sdf(vec3 point) {
    float d = INFINITY;
    for (int i = start_sphere; i < end_sphere; i++) {
        if (sphere_include[i]) {
            float sd = length(point - sphere_positions[i]) - sphere_radii[i];
            if (isinf(d)) {
                d = sd;
            } else if (smoothing > 0.0) {
                float h = clamp(0.5 + 0.5*(sd-d)/smoothing, 0.0, 1.0);
                d = mix(sd, d, h) - smoothing*h*(1.0-h);
            } else if (sd < d) {
                d = sd;
            }
        }
    }
    return d;
}

// ...and one that also calculates the color.
//
vec4 scene_sdf_rgb(vec3 point) {
    float d = INFINITY;
    vec3 rgb = vec3(0.0);
    for (int i = start_sphere; i < end_sphere; i++) {
        if (sphere_include[i]) {
            float sd = length(point - sphere_positions[i]) - sphere_radii[i];
            if (isinf(d)) {
                d = sd;
                rgb = sphere_colors[i];
            } else if (smoothing > 0.0) {
                float h = clamp(0.5 + 0.5*(sd-d)/smoothing, 0.0, 1.0);
                d = mix(sd, d, h) - smoothing*h*(1.0-h);
                rgb = mix(sphere_colors[i], rgb, h);
            } else if (sd < d) {
                d = sd;
                rgb = sphere_colors[i];
            }
        }
    }
    return vec4(rgb, d);
}

// Our ray marcher does standard spherical marching using the distance-only
// function start_sphere and then does one more iteration with the color function
// and 6 more calls to the distance function to calculate a surface normal.
//
Trace ray_march(vec3 origin, vec3 direction, float max_distance) {
    if (max_distance == 0.0) {
        return Trace(INFINITY, vec3(0.0), vec3(0.0), 0);
    }
    int i = 1;
    vec3 p = origin;
    float d = 0.0;
    float delta = INFINITY;
    for (; i < max_iterations && abs(delta) > epsilon; i++) {
        delta = scene_sdf(p);
        d += delta;
        if (d > max_distance) {
            return Trace(INFINITY, vec3(0.0), vec3(0.0), i);
        }
        p = origin + d * direction;
    }
    vec4 rgbd = scene_sdf_rgb(p);
    d += rgbd.a;
    p = origin + d * direction;
    vec3 d1 = vec3(scene_sdf(p + NORMAL_DELTA.xyy), scene_sdf(p + NORMAL_DELTA.yxy), scene_sdf(p + NORMAL_DELTA.yyx)),
         d2 = vec3(scene_sdf(p - NORMAL_DELTA.xyy), scene_sdf(p - NORMAL_DELTA.yxy), scene_sdf(p - NORMAL_DELTA.yyx));
    return Trace(d, rgbd.rgb, normalize(d1 - d2), i);
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
    // We ray-march from the container fragment position away from the viewer
    // to find an intersection with the SDF scene. If there is no hit then we
    // discard this fragment. Otherwise we use the normal lighting calculation
    // with a new world position, normal and emissive colour derived from the
    // SDF functions, and update the fragment depth to match the SDF surface
    // depth.

    // We do a quick pass through all the spheres first doing a ray/sphere
    // intersection, with the spheres expanded by 1.5x the smoothing amount to
    // account for how much they can be distorted by the smoothing logic. This
    // gives us a set of possible spheres that this ray may intersect with. At
    // the same time we work out (conservative) bounds for the minimum and
    // maximum distance that an intersection could occur at, this allows us to
    // start and stop the raymarcher at those distances.
    //
    int sphere_count = 0;
    start_sphere = -1;
    end_sphere = 0;
    float max_distance = 0.0;
    for (int i = 0; i < NSPHERES; i++) {
        vec3 p = sphere_positions[i] - world_position;
        float d = dot(-V, p);
        float r = sphere_radii[i] + smoothing*1.5;
        if (length(p + d * V) < r) {
            max_distance = max(max_distance, d);
            sphere_include[i] = true;
            if (start_sphere == -1) {
                start_sphere = i;
            }
            end_sphere = i + 1;
            sphere_count++;
        } else {
            sphere_include[i] = false;
        }
    }

    // Ray-march to find the actual position, normal and emissive colour of this
    // fragment.
    //
    Trace trace = ray_march(world_position, -V, max_distance);
% if show_cost:
    float a = float(trace.i) / float(max_iterations);
    float b = float(sphere_count) / float(NSPHERES);
    fragment_color = vec4(a, a*b, b, 1.0);
% else:
    if (isinf(trace.d)) {
        discard;
    }
    vec3 emissive = trace.c;
    vec3 world_norm = trace.n;
    view_distance += trace.d;
    vec3 world_pos = world_position - V*trace.d;

    // The rest of the material properties and lighting calculation is done
    // as per usual.
    //
    float fog_alpha = (fog_max > fog_min) && (fog_curve > 0.0) ? pow(clamp((view_distance - fog_min) / (fog_max - fog_min), 0.0, 1.0), 1.0/fog_curve) : 0.0;
    vec3 albedo = fragment_albedo.rgb;
    float transparency = fragment_albedo.a;
    float ior = fragment_properties.x;
    float metal = fragment_properties.y;
    float roughness = fragment_properties.z;
    float ao = fragment_properties.w;

    vec3 transmission_color = vec3(0.0);
    vec3 diffuse_color = vec3(0.0);
    vec3 specular_color = emissive;
    compute_pbr_lighting(world_pos, world_norm, V,
                         ior, roughness, metal, ao, albedo,
                         transmission_color, diffuse_color, specular_color);

    float opacity = 1.0 - transparency;
    vec3 final_color = mix(diffuse_color, fog_color, fog_alpha) * opacity + specular_color * (1.0 - fog_alpha);
    if (monochrome) {
        final_color = vec3(srgb_luminance(final_color));
    }
    fragment_color = vec4(final_color * tint, opacity);

    // Update the fragment depth from the position of the SDF surface.
    //
    vec4 p = pv_matrix * vec4(world_pos, 1);
    gl_FragDepth = gl_DepthRange.diff * (p.z / p.w + 1.0) * 0.5 + gl_DepthRange.near;
% endif
}
