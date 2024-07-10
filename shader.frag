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
    vec3 A; // ambient
    float D; // diffuse
    float P; // specular
    float S; // shininess
    float R; // reflection
    vec3 C; // RGB color
};

float DRAW_DISTANCE = 5000.0;
float HALF_PI = acos(0);
float building_modulo = 200;
float time = 0;

float origin_box(vec3 p, vec3 dimensions, float corner_radius) {
    vec3 a = abs(p);
    return length(max(abs(p) - dimensions, 0.0)) - corner_radius;
}

mat2 rotate(float degrees) {
    float a = degrees * HALF_PI / 90;
    return mat2(cos(a), -sin(a), sin(a), cos(a));
}

float coarse_box(vec3 p, vec3 dimensions, float corner_radius, float coarseness) {
    vec3 a = abs(p);
    vec3 q = p;
    q.xz *= rotate(40);
    q.yz *= rotate(50);
    return length(max(abs(p) - dimensions, 0.0)) - corner_radius
        + coarseness * sin(q.x * (101 + q.y * (163 + q.z)));
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
            -skyscraper_windows(p, max_floors, vec2(0.7, 0.5), window_modulo));
}

float skyscraper_interior(vec3 p, vec3 dimensions) {
    return origin_box(p, dimensions, 0.1);
}

vec3 building_rgb(vec3 p, float building_seed) {
    float seed = mod(building_seed, 1000) + 1;
    float base = 0.1 + mod(seed / 100, 0.2);
    return vec3(
        pow(base, 1 + 0.05*sin(seed)),
        pow(base, 1 + 0.05*sin(seed/1e2)),
        pow(base, 1 + 0.05*sin(seed/1e4)));
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
    float ambience_exponent = 0.1 + abs(mod(building_seed, 9.5));
    ambience_exponent /= abs(mod(abs(divy * (9967 + divy * 9973)), 3)); // brighten some floors
    float ambience = pow(a, ambience_exponent);
    // maybe use some color temperature formula for the tint? keep it simple for now
    vec3 tint = vec3(
        pow(0.6, 2 + 2*sin(building_seed)),
        pow(0.6, 2 + 2*sin(building_seed/1e2)),
        pow(0.6, 2 + 2*sin(building_seed/1e4)));
    vec3 rgb = vec3(
        pow(tint.r, 1 + sin(seed)),
        pow(tint.g, 1 + sin(seed*2)),
        pow(tint.b, 1 + sin(seed*3)));
    return ma(ambience * rgb, 0.9, 0.98, 15, 0, building_rgb(p, building_seed));
}

ma building_material(vec3 p, float building_seed) {
    vec3 rgb = building_rgb(p, building_seed);
    return ma(0.1 * rgb, 0.9, 0, 10, 0, rgb);
}

void closest_material(inout float dist, inout ma mat, float new_dist, ma new_mat) {
    if (new_dist < dist) {
        dist = new_dist;
        mat = new_mat;
    }
}

float ground(vec3 p) {
    float cliff = max(
        p.y + 1,
        p.z + building_modulo / 4);
    return cliff;
}

float sea(vec3 p) {
    p.xz *= rotate(-10);
    float wave1 = 0.2 * sin((p.xz * rotate(-10)).y / 4 + 3.1*time);
    float wave2 = 0.15 * sin((p.xz * rotate(5)).y / 3.9 + 2.1*time);
    float wave3 = 0.003 * sin((p.xz * rotate(-35)).y / 0.4 + 5*time);
    float wave4 = 0.002 * sin((p.xz * rotate(40)).y / 0.3 + 3*time);
    return p.y + 10 + wave1 + wave2 + wave3 + wave4;
}

void skyscraper(vec3 p, inout float dist, inout ma mat, vec3 dimensions, float building_seed) {
    float window_modulo = 2;
    closest_material(dist, mat, skyscraper_exterior(p, dimensions, window_modulo), building_material(p, building_seed));
    closest_material(dist, mat, skyscraper_interior(p, dimensions - vec3(0.1)), window_material(p, window_modulo, building_seed));
}

float round_odd(float x) {
    x = round(x);
    return x + 1 - mod(x, 2);
}

void repeated_skyscrapers(vec3 p, inout float dist, inout ma mat, float base_seed, float min_zdiv) {
    float divx = floor((p.x - 0.5 * building_modulo) / building_modulo);
    float divz = min(min_zdiv, floor((p.z - 0.5 * building_modulo) / building_modulo));
    float building_seed = base_seed + round(9949 * (divx + 9967 * divz));
    building_seed += round(9949 * (divz + 9967 * divx));
    float height_adjustment = 200 / (1 + pow(abs(divx), 1.2));
    p.x -= (divx + 1) * building_modulo;
    p.z -= (divz + 1) * building_modulo;
    float height = round_odd(50 + height_adjustment + mod(abs(building_seed), 19));
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
    float offset = building_modulo/2;
    float seed = 1337;
    repeated_skyscrapers(p, dist, mat, pseudo_random(seed), -2);
    repeated_skyscrapers(p + vec3(offset,0,0), dist, mat, pseudo_random(seed), -2);
    repeated_skyscrapers(p + vec3(0,0,offset), dist, mat, pseudo_random(seed), -1);
    repeated_skyscrapers(p + vec3(offset,0,offset), dist, mat, pseudo_random(seed), -1);
}

float waterfront_stairs(vec3 p) {
    //p.z += 1;
    p.z += building_modulo / 4 + 9;
    p.y += 23.5;
    float dist = length(max(abs(p.yz) - vec2(22, 20), 0.0)) - 0.5;
    for (int i = 0; i < 3; i++) {
        p.y += 2.5;
        p.z -= 20;
        dist = min(dist, length(max(abs(p.yz) - vec2(22, 20), 0.0)) - 0.5);
    }
    return dist;
}

void city(vec3 p, inout float dist, inout ma mat) {
    repeated_random_buildings(p, dist, mat);
}

float FERRIS_WHEEL_RADIUS = 100;

float wheel(vec3 p) {
    vec2 q = vec2(length(p.xy) - FERRIS_WHEEL_RADIUS, p.z);
    return length(q) - 1;
}

float wheel_spike(vec3 p) {
    p.x -= clamp(p.x, 0, FERRIS_WHEEL_RADIUS);
    return length(p) - 0.5;
}

float wheel_spikes(vec3 p) {
    // polar coordinates
    p.xy *= rotate(0.1); // workaround for disappearing vertical spikes
    float polar_r = length(p.xy);
    float polar_a = atan(p.y, p.x);
    float angle = 30;
    float modulo = angle * HALF_PI / 90; // to radians
    // repeated wheel spike in polar coordinates
    polar_a = mod(polar_a - 0.5 * modulo, modulo) - 0.5 * modulo;
    vec3 q = vec3(polar_r * cos(polar_a), polar_r * sin(polar_a), p.z);
    return wheel_spike(q);
}

float wheel_support(vec3 p) {
    p.z = abs(p.z) - 5;
    p.x = abs(p.x);
    p.xy *= rotate(70);
    p.x -= clamp(p.x, 0, FERRIS_WHEEL_RADIUS * 1.25);
    return length(p) - 1;
}

float wheel_dist(vec3 p) {
    p.xy *= rotate(2.5 * time);
    return min(wheel(p), wheel_spikes(p));
}

vec3 wheel_color(vec3 p) {
    float polar_r = length(p.xy);
    float polar_a = atan(p.y, p.x);
    polar_r = FERRIS_WHEEL_RADIUS - polar_r;
    polar_r /= 2;
    float r = 0.6 + 0.4 * sin(polar_r + HALF_PI);
    float g = 0.6 + 0.4 * sin(polar_r * 2 - HALF_PI);
    float b = 0.6 + 0.4 * sin(polar_r / 2);
    return vec3(r, g, b);
}

void ferris_wheel(vec3 p, inout float dist, inout ma mat) {
    p.z += 30;
    p.x += 700;
    p.y -= FERRIS_WHEEL_RADIUS + 10;
    vec3 col = wheel_color(p);
    closest_material(dist, mat, wheel_dist(p), ma(0.9 * col, 0.1, 0, 10, 0, col));
    col = vec3(0.7, 1, 0.7);
    closest_material(dist, mat, wheel_support(p), ma(0.9 * col, 0.1, 0, 10, 0, col));
}

float light_pole(vec3 p) {
    p.y += 10;
    p.y -= clamp(p.y, 0, 12);
    return length(p) - 0.5;
}

float street_light_modulo = 20;

vec3 closest_street_light_pos(vec3 p) {
    return vec3(
        round(p.x / street_light_modulo) * street_light_modulo,
        10,
        -building_modulo / 4 - 5);
}

void street_lights(vec3 p, inout float dist, inout ma mat) {
    p.x = mod(p.x - 0.5 * street_light_modulo, street_light_modulo) - 0.5 * street_light_modulo;
    p.z += building_modulo / 4 + 5;
    p.y -= 10;
    float light = length(p) - 2;
    vec3 light_color = vec3(1, 1, 0.5);
    closest_material(dist, mat, light, ma(0.9 * light_color, 0.1, 0, 10, 0, light_color));
    closest_material(dist, mat, light_pole(p), ma(vec3(0.1), 0.9, 0, 10, 0, vec3(0.2)));
}

float bridge_coarseness = 2e-4;

float bridge_road(vec3 p) {
    p.x = abs(p.x) - 10;
    return coarse_box(p, vec3(8.2, 1, 1000), 0.2, bridge_coarseness);
}

float repeated_fence_pattern(vec2 p) {
    vec2 hole_dimensions = vec2(0.9, 1.5);
    float modulo = 2;
    p.x = mod(p.x - 0.5 * modulo, modulo) - 0.5 * modulo;
    return length(max(abs(p) - hole_dimensions, 0.0)) - 0.01;
}

float bridge_fences(vec3 p) {
    p.x = abs(p.x) - 18;
    p.y -= 3.5;
    return max(
        origin_box(p, vec3(0.1, 2, 1000), 0.1),
        -repeated_fence_pattern(p.zy));
}

float bridge_pillars(vec3 p) {
    p.x = abs(p.x) - 20;
    p.z = abs(p.z) - 150;
    float dist = coarse_box(p, vec3(5, 80, 2), 0.2, bridge_coarseness);
    p.x += 15;
    vec3 q = p;
    q.y -= 60;
    q.y = abs(q.y) - 20;
    dist = min(dist, coarse_box(q, vec3(20, 5, 2), 0.2, bridge_coarseness));
    p.y += 40;
    dist = min(dist, coarse_box(p, vec3(20, 3, 4), 0.5, bridge_coarseness));
    return dist;
}

float bridge_wires(vec3 p) {
    float orig_z = p.z;
    float modulo = 300;
    p.x = abs(p.x) - 21;
    float radius = 175;
    p.y -= radius + 3;
    p.z = mod(p.z - 0.5 * modulo, modulo) - 0.5 * modulo;
    vec2 q = vec2(length(p.yz) - radius, p.x);
    float dist = length(q) - 1;
    dist = max(dist, p.y + radius - 83);
    dist = max(dist, orig_z - modulo);
    dist = max(dist, -orig_z - modulo);
    return dist;
}

float bridge_geom(vec3 p) {
    float dist = bridge_road(p);
    dist = min(dist, bridge_fences(p));
    dist = min(dist, bridge_pillars(p));
    dist = min(dist, bridge_wires(p));
    return dist;
}

float bridge_light_modulo = 30;
float bridge_light_offset = 8;
float bridge_x_offset = -building_modulo / 4;
float bridge_y_offset = 30;

vec3 closest_bridge_light_pos(vec3 p, float light_x_offset) {
    return vec3(
        bridge_x_offset + light_x_offset,
        bridge_y_offset + 10,
        round(p.z / bridge_light_modulo) * bridge_light_modulo);
}

void bridge_street_lights(vec3 p, inout float dist, inout ma mat) {
    p.z = mod(p.z - 0.5 * bridge_light_modulo, bridge_light_modulo) - 0.5 * bridge_light_modulo;
    p.y -= bridge_y_offset + 10;
    p.x -= bridge_x_offset;
    p.x = abs(p.x) - bridge_light_offset * 2;
    float light = length(p) - 2;
    vec3 light_color = vec3(0.8, 0.9, 1);
    closest_material(dist, mat, light, ma(0.9 * light_color, 0.1, 0, 10, 0, light_color));
    closest_material(dist, mat, light_pole(p), ma(vec3(0.1), 0.9, 0, 10, 0, vec3(0.2)));
}

void bridge(vec3 p, inout float dist, inout ma mat) {
    p.z -= 500;
    p.x -= bridge_x_offset;
    p.y -= bridge_y_offset;
    vec3 bridge_color = vec3(1, 0.8, 0.6);
    closest_material(dist, mat, bridge_geom(p), ma(bridge_color * 0.1, 0.9, 0, 10, 0, bridge_color));
}

// Simplified scene for shadow calculations, should not contain light source geometries.
float shadow_scene(vec3 p, out ma mat) {
    float dist = ground(p);
    mat = ma(vec3(0.1), 0.9, 0, 10, 0.0, vec3(0.8));
    city(p, dist, mat);
    closest_material(dist, mat, waterfront_stairs(p), ma(vec3(0.1), 0.9, 0, 10, 0, vec3(0.7)));
    bridge(p, dist, mat);
    //ferris_wheel(p, dist, mat);
    //street_lights(p, dist, mat);
    //closest_material(dist, mat, sea(p), ma(0.1, 0.9, 0, 10, 0.7, vec3(0.1, 0.1, 0.3)));
    return dist;
}

float scene(vec3 p, out ma mat) {
    float dist = shadow_scene(p, mat);
    ferris_wheel(p, dist, mat);
    street_lights(p, dist, mat);
    bridge_street_lights(p, dist, mat);
    closest_material(dist, mat, sea(p), ma(vec3(0.1), 0.9, 0.94, 15, 0.7, vec3(0.1, 0.1, 0.3)));
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

float soft_shadow(vec3 p, vec3 light_direction, float max_distance, float sharpness) {
    ma m;
    p += light_direction * 0.1;
    float total_dist = 0.1;
    float res = 1.0;
    for (int i = 0; i < 100; i++) {
        float dist = shadow_scene(p, m);
        if (dist < 0.01) {
            return 0.0;
        }
        total_dist += dist;
        res = min(res, sharpness * dist / total_dist);
        if (total_dist > max_distance) {
            break;
        }
        p += light_direction * dist;
    }
    return res;
}

vec3 background_color(vec3 direction) {
    float y = direction.y;
    float x = direction.x;
    vec3 sky = vec3(
        0.1 - 0.3 * y + 0.2 * cos(x * 3),
        0.1 - 0.1 * y,
        0.05 + 0.6 * y);
    return sky;
}

vec3 apply_fog(vec3 color, float total_distance, vec3 direction) {
    return mix(color, background_color(direction), 1.0 - exp(-0.0006 * total_distance));
}

vec3 phong_lighting(vec3 p, ma mat, vec3 ray_direction) {
    vec3 normal = estimate_normal(p);
    vec3 light_positions[] = {
        vec3(1000, 2000, 1000),
        closest_street_light_pos(p - vec3(street_light_modulo, 0, 0)),
        closest_street_light_pos(p),
        closest_street_light_pos(p + vec3(street_light_modulo, 0, 0)),
        closest_bridge_light_pos(p - vec3(0, 0, -bridge_light_modulo), bridge_light_offset),
        closest_bridge_light_pos(p, bridge_light_offset),
        closest_bridge_light_pos(p + vec3(0, 0, -bridge_light_modulo), bridge_light_offset),
        closest_bridge_light_pos(p - vec3(0, 0, -bridge_light_modulo), -bridge_light_offset),
        closest_bridge_light_pos(p, -bridge_light_offset),
        closest_bridge_light_pos(p + vec3(0, 0, -bridge_light_modulo), -bridge_light_offset),
        };
    vec3 light_colors[] = {
        vec3(0.15),
        vec3(1, 1, 0.5),
        vec3(1, 1, 0.5),
        vec3(1, 1, 0.5),
        vec3(0.8, 0.9, 1),
        vec3(0.8, 0.9, 1),
        vec3(0.8, 0.9, 1),
        vec3(0.8, 0.9, 1),
        vec3(0.8, 0.9, 1),
        vec3(0.8, 0.9, 1),
        };
    float street_light_dropoff = -0.01;
    float bridge_light_dropoff = -0.03;
    float light_dropoff[] = {
        -1e-5,
        street_light_dropoff, street_light_dropoff, street_light_dropoff,
        bridge_light_dropoff, bridge_light_dropoff, bridge_light_dropoff,
        bridge_light_dropoff, bridge_light_dropoff, bridge_light_dropoff,
        };
    vec3 diffuse_and_specular_sum = vec3(0);
    for (int i = 0; i < 10; i++) {
        vec3 light_pos = light_positions[i];
        vec3 light_color = light_colors[i];
        vec3 light_direction = normalize(p - light_pos);
        float light_distance = length(p - light_pos);
        float light_intensity = exp(light_dropoff[i] * light_distance);
        float shadow = soft_shadow(p, -light_direction, light_distance, 40.0);
        float diffuse = max(0.0, mat.D * dot(normal, -light_direction)) * shadow * light_intensity;
        vec3 reflection = ray_reflection(ray_direction, normal);
        float specular = pow(max(0.0, mat.P * dot(reflection, -light_direction)), mat.S) * shadow;
        diffuse_and_specular_sum += (mat.C * diffuse * light_color) + vec3(specular);
    }
    return min(mat.A + diffuse_and_specular_sum, vec3(1.0));
}

vec3 apply_reflections(vec3 color, ma mat, vec3 p, vec3 direction) {
    float reflection = mat.R;
    for (int i = 0; i < 3; i++) {
        if (reflection <= 0.01) {
            break;
        }
        direction = ray_reflection(direction, estimate_normal(p));
        vec3 reflection_color = background_color(direction);
        vec3 start_pos = p;
        p += 0.05 * direction;
        if (ray_march(p, direction, mat)) {
            reflection_color = phong_lighting(p, mat, direction);
            reflection_color = apply_fog(reflection_color, length(p - start_pos), direction);
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
    //vec3 eye_position = vec3(-30, 50, 700);
    vec3 eye_position = vec3(30, -3, 690);
    vec3 forward = normalize(vec3(-180, 2, -3) - eye_position);
    //vec3 eye_position = vec3(350, 50, 350);
    //vec3 forward = normalize(vec3(-50, 6, 300) - eye_position);
    vec3 up = vec3(0.0, 1.0, 0.0);
    vec3 right = normalize(cross(up, forward));
    up = cross(-right, forward);
    float focal_length = 1.0;
    vec3 start_pos = eye_position + forward * focal_length + right * u + up * v;
    vec3 direction = normalize(start_pos - eye_position);
    vec3 p = start_pos;
    vec3 color = background_color(direction);
    ma mat;
    if (ray_march(p, direction, mat)) {
        color = phong_lighting(p, mat, direction);
        color = apply_reflections(color, mat, p, direction);
        color = apply_fog(color, length(p - start_pos), direction);
    }
    return color;
}

vec3 render_long_exposure(float u, float v, float total_time, int timesteps) {
    vec3 r_sum = vec3(0);
    vec3 r_max = vec3(0);
    float time_delta = total_time / timesteps;
    float angle_step = HALF_PI * 4.0 / timesteps;
    float epsilon = 0.2 / max(SIZE.x, SIZE.y);
    for (int i = 0; i < timesteps; i++) {
        time = i * time_delta;
        // sneak in some antialiasing by adding small epsilons to u/v
        vec3 tmp = render(
            u + epsilon * cos(angle_step * i),
            v + epsilon * sin(angle_step * i));
        r_sum += tmp;
        r_max = max(r_max, tmp);
    }
    vec3 r_avg = r_sum / timesteps;
    // It's not obvious if the average or the maximum is best, so... whatever, just mix them :-)
    return mix(r_avg, r_max, 0.5);
}

void main() {
    float u = C.x - 1.0;
    float v = (C.y - 1.0) * SIZE.y / SIZE.x;
#ifdef SPIRV
    vec3 F;
#endif
    // shader minifier bug: make sure total time is a float literal to avoid integer division
    F = render_long_exposure(u, v, 7.0, 16);
    // vignette
    //float edge = abs(C.x - 1) + abs(C.y - 1);
    //F = mix(F, vec3(0), min(1, max(0, edge*0.3 - 0.2)));
#ifdef SPIRV
    outColor = vec4(F, 0);
#endif
}
