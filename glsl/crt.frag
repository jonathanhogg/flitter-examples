${HEADER}

// This is a fairly simple cathode ray tube effect shader that simulates an
// RGB display with a slot shadow mask. It only expects one child/input so
// you'll need to use a compositing shader in between if you have multiple
// inputs.
//
// `scale` is the ratio of the actual framebuffer size to the shadow mask size.
// It should be a multiple of 3 ideally and probably 9 or more for the best
// effect. `brightness` is how much to oversaturate the individual RGB dots.
// The dots are graduated at the edges and so this can be set to 2 without any
// horrible colour shifting. Adding a `!bloom` with a small `radius` (1/3rd of
// `scale` is quite reasonable) gives a nice glow to the pixels and allows you
// to safely push `brightness` up to about 3.
//
// The effect actually looks much nicer when massively overscaled. The mask is
// aligned to the centre of the screen so you can actually smoothly change the
// value of `scale` to zoom the mask. If you want to reproduce the effect of
// zooming in or out on the image, then you'll also need to scale the input
// image with an intermediate `!transform scale=` node. If you're using a large
// `scale` then you'll likely also want to add `repeat=false` to clamp sampling
// to the edges of the input texture. That will ensure that the partial pixels
// at the edge of the screen render sensibly.

in vec2 coord;
out vec4 color;

uniform float scale;
uniform float brightness;
uniform vec2 size;
uniform sampler2D texture0;

void main() {
    vec2 input_size = size / scale;
    vec2 p = (coord - 0.5) * input_size + 0.5;
    vec2 q = floor(p);
    vec4 c = texture(texture0, q / input_size + 0.5);
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
            c = mix(c, texture(texture0, vec2(q.x, q.y + 1.0) / input_size + 0.5), p.y);
        } else {
            c = mix(texture(texture0, vec2(q.x, q.y - 1.0) / input_size + 0.5), c, p.y);
        }
    }
    vec4 mask = vec4(0.0);
    float g = fract(p.x);
    mask[int(floor(p.x)) % 3] = 16.0 * brightness * p.y * (1.0 - p.y) * g * (1.0 - g);
    color = vec4(c.rgb * mask.rgb, c.a);
}
