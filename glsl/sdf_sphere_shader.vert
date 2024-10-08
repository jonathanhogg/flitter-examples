${HEADER}

in vec3 model_position;
in vec3 model_normal;
in vec2 model_uv;
in mat4 model_matrix;
in mat3 model_normal_matrix;

in vec4 material_albedo;
in vec4 material_properties;

out vec3 world_position;

flat out vec4 fragment_albedo;
flat out vec4 fragment_properties;

uniform mat4 pv_matrix;

void main() {
    world_position = (model_matrix * vec4(model_position, 1.0)).xyz;
    gl_Position = pv_matrix * vec4(world_position, 1.0);
    fragment_albedo = material_albedo;
    fragment_properties = material_properties;
}
