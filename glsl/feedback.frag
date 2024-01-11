
#version 330

in vec2 coord;
out vec4 color;

uniform sampler2D last;
uniform sampler2D texture0;
uniform vec2 size;
uniform float delta;

uniform float decay = 0.0;
uniform float glow = 0.0;
uniform vec2 translate = vec2(0.0, 0.0);
uniform vec2 scale = vec2(1.0, 1.0);
uniform float rotate = 0.0;

const float TwoPI = 6.283185307179586;


void main()
{
    float th = TwoPI * rotate * delta, cth = cos(th), sth = sin(th);
    vec2 last_coord = (coord - 0.5) * size / pow(scale, vec2(delta));
    last_coord *= mat2(cth, -sth, sth, cth);
    last_coord -= translate * vec2(delta, -delta);
    float k = decay > 0 ? exp(-delta/decay) : 0;
    color = mix(texture(texture0, coord) * (1.0 + glow), clamp(texture(last, last_coord / size + 0.5), 0, 1), k);
}
