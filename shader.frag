#version 450
#if defined(SPIRV)
layout(location=0) in vec2 C;
layout(location=0) out vec4 outColor;
layout(std140, binding=0) uniform X {
    vec2 SIZE;
};
#else
layout (location=0) in vec2 C;
out vec3 F;
layout (location=0) uniform vec2 SIZE;
#endif

// Shader minifier does not (currently) minimize structs, so use short names.
// Using a one-letter name for the struct itself seems to trigger a bug, so use two.
struct ma {
    float A; // ambient
    float D; // diffuse
    float P; // specular
    float S; // shininess
    float R; // reflection
    vec3 C; // RGB color
};

float DRAW_DISTANCE = 5000.0;

float horizontal_plane(vec3 p, float height) {
    return p.y - height;
}

float origin_box(vec3 p, vec3 dimensions, float corner_radius) {
    vec3 a = abs(p);
    return length(max(abs(p) - dimensions, 0.0)) - corner_radius;
}

float repeated_windows(vec2 p, vec2 dimensions, float modulo) {
    p = mod(p - 0.5 * modulo, modulo) - 0.5 * modulo;
    return length(max(abs(p) - dimensions, 0.0)) - 0.01;
}

float skyscraper_windows(vec3 p, float max_floors, vec2 window_dimensions, float modulo) {
    float d_xy = repeated_windows(p.xy, window_dimensions, modulo);
    float d_yz = repeated_windows(p.yz, window_dimensions, modulo);
    float d = min(d_xy, d_yz);
    return max(d, p.y - max_floors * modulo - 0.5 * modulo);
}

float skyscraper_exterior(vec3 p, vec3 dimensions, float window_modulo) {
    float max_floors = floor(dimensions.y / window_modulo) - 1;
    return
        max(
            origin_box(p, dimensions, 0.1),
            -skyscraper_windows(p, max_floors, vec2(0.6, 0.5), window_modulo));
}

float skyscraper_interior(vec3 p, vec3 dimensions) {
    return origin_box(p, dimensions, 0.1);
}

ma window_material(vec3 p, float modulo, float building_seed) {
    // pseudorandom based on window position
    float divx = floor((p.x - 0.5 * modulo) / modulo);
    float divy = floor((p.y - 0.5 * modulo) / modulo);
    float divz = floor((p.z - 0.5 * modulo) / modulo);
    float seed = round(9949 * (divx + 9967 * (divy + 9973 * divz)));
    seed += round(9949 * (divz + 9967 * (divy + 9973 * divx)));
    // higher exponents makes windows darker
    float a = 0.5 + 0.5 * sin(mod(seed, 1000));
    float ambience_exponent = 5 + abs(mod(building_seed, 10));
    ambience_exponent /= abs(mod(abs(divy * (9967 + divy * 9973)), 3)); // brighten some floors
    float ambience = pow(a, ambience_exponent);
    // maybe use some color temperature formula for the tint? keep it simple for now
    vec3 tint = vec3(
        pow(0.9, 1 + sin(building_seed)),
        pow(0.9, 1 + sin(building_seed/1e2)),
        pow(0.6, 1 + sin(building_seed/1e4)));
    vec3 rgb = vec3(
        pow(tint.r, 1 + sin(seed)),
        pow(tint.g, 1 + sin(seed*2)),
        pow(tint.b, 1 + sin(seed*3)));
    return ma(ambience, 0.1, 0, 10, 0, rgb);
}

void closest_material(inout float dist, inout ma mat, float new_dist, ma new_mat) {
    if (new_dist < dist) {
        dist = new_dist;
        mat = new_mat;
    }
}

float ground(vec3 p) {
    return horizontal_plane(p, -1);
}

void skyscraper(vec3 p, inout float dist, inout ma mat, vec3 dimensions, float building_seed) {
    float window_modulo = 2;
    closest_material(dist, mat, skyscraper_exterior(p, dimensions, window_modulo), ma(0.1, 0.9, 0, 10, 0, vec3(0.1)));
    closest_material(dist, mat, skyscraper_interior(p, dimensions - vec3(0.1)), window_material(p, window_modulo, building_seed));
}

float round_odd(float x) {
    x = round(x);
    return x + 1 - mod(x, 2);
}

void repeated_skyscrapers(vec3 p, inout float dist, inout ma mat, float building_modulo, float base_seed) {
    float divx = floor((p.x - 0.5 * building_modulo) / building_modulo);
    float divz = floor((p.z - 0.5 * building_modulo) / building_modulo);
    float building_seed = base_seed + round(9949 * (divx + 9967 * divz));
    building_seed += round(9949 * (divz + 9967 * divx));
    p.x = mod(p.x - building_modulo * 0.5, building_modulo) - building_modulo * 0.5;
    p.z = mod(p.z - building_modulo * 0.5, building_modulo) - building_modulo * 0.5;
    float height = round_odd(150 + mod(building_seed, 50));
    float size_seed = round(building_seed / 50);
    float width = round_odd(10 + mod(size_seed, 25));
    size_seed = round(size_seed / 25);
    float depth = round_odd(10 + mod(size_seed, 25));
    vec3 dimensions = vec3(width, height, depth);
    skyscraper(p, dist, mat, dimensions, building_seed);
}

float pseudo_random(inout float seed) {
    seed = round(mod(seed * 127, 123453));
    return seed;
}

void repeated_random_buildings(vec3 p, inout float dist, inout ma mat) {
    float building_modulo = 200;
    float offset = building_modulo/2;
    float seed = 1337;
    repeated_skyscrapers(p, dist, mat, building_modulo, pseudo_random(seed));
    repeated_skyscrapers(p + vec3(offset,0,0), dist, mat, building_modulo, pseudo_random(seed));
    repeated_skyscrapers(p + vec3(0,0,offset), dist, mat, building_modulo, pseudo_random(seed));
    repeated_skyscrapers(p + vec3(offset,0,offset), dist, mat, building_modulo, pseudo_random(seed));
}

float scene(vec3 p, out ma mat) {
    float dist = ground(p);
    mat = ma(0.1, 0.9, 0, 10, 0.0, vec3(0.8));
    repeated_random_buildings(p, dist, mat);
    return dist;
}

bool ray_march(inout vec3 p, vec3 direction, out ma material) {
    float total_dist = 0.0;
    for (int i = 0; i < 5000; i++) {
        float dist = scene(p, material);
        if (dist < 0.001) {
            return true;
        }
        total_dist += dist;
        if (total_dist > DRAW_DISTANCE) {
            return false;
        }
        p += direction * dist;
    }
    return false;
}

vec3 estimate_normal(vec3 p) {
    float epsilon = 0.001;
    ma m;
    return normalize(vec3(
        scene(vec3(p.x + epsilon, p.y, p.z), m) - scene(vec3(p.x - epsilon, p.y, p.z), m),
        scene(vec3(p.x, p.y + epsilon, p.z), m) - scene(vec3(p.x, p.y - epsilon, p.z), m),
        scene(vec3(p.x, p.y, p.z + epsilon), m) - scene(vec3(p.x, p.y, p.z - epsilon), m)
    ));
}

vec3 ray_reflection(vec3 direction, vec3 normal) {
    return 2.0 * dot(-direction, normal) * normal + direction;
}

float soft_shadow(vec3 p, vec3 light_direction, float sharpness) {
    ma m;
    p += light_direction * 0.1;
    float total_dist = 0.1;
    float res = 1.0;
    for (int i = 0; i < 100; i++) {
        float dist = scene(p, m);
        if (dist < 0.01) {
            return 0.0;
        }
        total_dist += dist;
        res = min(res, sharpness * dist / total_dist);
        if (total_dist > DRAW_DISTANCE) {
            break;
        }
        p += light_direction * dist;
    }
    return res;
}

const vec3 background_color = vec3(0.3, 0.2, 0.15);

vec3 apply_fog(vec3 color, float total_distance) {
    return mix(color, background_color, 1.0 - exp(-0.001 * total_distance));
}

vec3 phong_lighting(vec3 p, ma mat, vec3 ray_direction) {
    vec3 normal = estimate_normal(p);
    vec3 light_direction = normalize(vec3(-0.3, -1.0, -0.5));
    float shadow = soft_shadow(p, -light_direction, 20.0);
    float diffuse = max(0.0, mat.D * dot(normal, -light_direction)) * shadow;
    vec3 reflection = ray_reflection(ray_direction, normal);
    float specular = pow(max(0.0, mat.P * dot(reflection, -light_direction)), mat.S) * shadow;
    return min(mat.C * (diffuse + mat.A) + vec3(specular), vec3(1.0));
}

vec3 apply_reflections(vec3 color, ma mat, vec3 p, vec3 direction) {
    float reflection = mat.R;
    for (int i = 0; i < 3; i++) {
        if (reflection <= 0.01) {
            break;
        }
        vec3 reflection_color = background_color;
        direction = ray_reflection(direction, estimate_normal(p));
        vec3 start_pos = p;
        p += 0.05 * direction;
        if (ray_march(p, direction, mat)) {
            reflection_color = phong_lighting(p, mat, direction);
            reflection_color = apply_fog(reflection_color, length(p - start_pos));
            color = mix(color, reflection_color, reflection);
            reflection *= mat.R;
        } else {
            color = mix(color, reflection_color, reflection);
            break;
        }
    }
    return color;
}

vec3 render(float u, float v) {
    vec3 eye_position = vec3(80, 250, 200);
    vec3 forward = normalize(vec3(0, 120, -3) - eye_position);
    vec3 up = vec3(0.0, 1.0, 0.0);
    vec3 right = normalize(cross(up, forward));
    up = cross(-right, forward);
    float focal_length = 1.0;
    vec3 start_pos = eye_position + forward * focal_length + right * u + up * v;
    vec3 direction = normalize(start_pos - eye_position);
    vec3 p = start_pos;
    vec3 color = background_color;
    ma mat;
    if (ray_march(p, direction, mat)) {
        color = phong_lighting(p, mat, direction);
        color = apply_reflections(color, mat, p, direction);
        color = apply_fog(color, length(p - start_pos));
    }
    return color;
}

vec3 render_aa(float u, float v) {
    // Antialiasing: render and blend 2x2 points per pixel.
    // That means the distance between points is 1/2 pixel,
    // and the distance from the center (du, dv) is 1/4 pixel.
    // Each pixel size is (2.0 / W, 2.0 / H) since the full area is -1 to 1.
    float du = 2.0 / SIZE.x / 4.0;
    float dv = 2.0 / SIZE.y / 4.0;
    vec3 sum =
        render(u - du, v - dv) +
        render(u - du, v + dv) +
        render(u + du, v - dv) +
        render(u + du, v + dv);
    return sum / 4;
}

void main() {
    float u = C.x - 1.0;
    float v = (C.y - 1.0) * SIZE.y / SIZE.x;
#ifdef SPIRV
    vec3 F;
#endif
#if defined(DEBUG)
    F = render(u, v);
#else
    F = render_aa(u, v);
#endif
    // vignette
    //float edge = abs(C.x - 1) + abs(C.y - 1);
    //F = mix(F, vec3(0), min(1, max(0, edge*0.3 - 0.2)));
#ifdef SPIRV
    outColor = vec4(F, 0);
#endif
}
