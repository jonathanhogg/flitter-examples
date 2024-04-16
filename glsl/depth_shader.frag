${HEADER}

// Copyright 2024 by Jonathan Hogg and licensed under the original Flitter
// BSD 2-clause license https://github.com/jonathanhogg/flitter/blob/main/LICENSE

in vec3 world_position;

out vec4 fragment_color;

uniform vec3 view_position;
uniform vec3 view_focus;
uniform bool orthographic;

void main() {
    float view_distance;
    if (orthographic) {
        view_distance = dot(view_position - world_position, normalize(view_position - view_focus));
    } else {
        view_distance = length(view_position - world_position);
    }
    fragment_color = vec4(view_distance, 0.0, 0.0, 1.0);
}
