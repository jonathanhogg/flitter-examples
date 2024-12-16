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
    vec2 q = floor(p);
    vec4 c = texture(texture0, q / input_size);
    vec2 grid = q;
    bool offset = int(grid.x) % 2 == 1;
    if (offset) {
        grid.y -= 0.5;
    }
    p -= grid;
    p.x *= 3.0;
    if (offset) {
        if (p.y > 1.0) {
            p.y -= 1.0;
            c = mix(c, texture(texture0, vec2(q.x, min(q.y + 1.0, input_size.y - 1.0)) / input_size), p.y);
        } else {
            c = mix(texture(texture0, vec2(q.x, max(0.0, q.y - 1.0)) / input_size), c, p.y);
        }
    }
    vec4 mask = vec4(0.0);
    float g = fract(p.x);
    mask[int(floor(p.x)) % 3] = 16.0 * brightness * p.y * (1.0 - p.y) * g * (1.0 - g);
    color = vec4(c.rgb * mask.rgb, c.a);
}
