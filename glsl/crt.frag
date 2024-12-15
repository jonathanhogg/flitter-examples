${HEADER}

in vec2 coord;
out vec4 color;

uniform float scale;
uniform vec2 size;
uniform float brightness;
uniform sampler2D texture0;

void main() {
    vec2 input_size = size / scale;
    vec2 p = coord * input_size;
    bool offset = int(floor(p.x)) % 2 == 0;
    if (offset) {
        p.y = max(0.0, p.y - 0.5);
    }
    vec2 q = floor(p);
    vec4 c = texture(texture0, q / input_size);
    float h = fract(p.y);
    if (offset) {
        c = mix(c, texture(texture0, vec2(q.x, min(q.y + 1.0, input_size.y - 1.0)) / input_size), h);
    }
    vec4 mask = vec4(0.0);
    mask[int(floor(p.x * 4.0)) % 4] = 4.0 * brightness * h * (1.0 - h);
    color = vec4(c.rgb * mask.rgb, c.a);
}
