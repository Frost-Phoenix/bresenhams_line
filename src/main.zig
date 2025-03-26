const rl = @import("raylib");

const scale = 5;
const world_size = 32;
const screen_size = world_size * scale;

pub fn main() !void {
    rl.initWindow(screen_size, screen_size, "Line drawing");
    defer rl.closeWindow();

    const render_texture = try rl.loadRenderTexture(world_size, world_size);
    defer rl.unloadRenderTexture(render_texture);

    rl.setTargetFPS(60);

    while (!rl.windowShouldClose()) {
        {
            rl.beginTextureMode(render_texture);
            defer rl.endTextureMode();

            rl.clearBackground(.light_gray);

            plotLine(10, 5, 23, 32);
        }

        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(.light_gray);

        const src: rl.Rectangle = .init(0, 0, world_size, -world_size);
        const dest: rl.Rectangle = .init(0, 0, screen_size, screen_size);

        rl.drawTexturePro(render_texture.texture, src, dest, .init(0, 0), 0, .white);
    }
}

fn plotLineLow(x0: i32, y0: i32, x1: i32, y1: i32) void {
    const dx: i32 = x1 - x0;
    var dy: i32 = y1 - y0;
    var yi: i32 = 1;

    if (dy < 0) {
        yi = -1;
        dy = -dy;
    }

    var D: i32 = (2 * dy) - dx;
    var y: i32 = y0;

    for (@intCast(x0)..@intCast(x1)) |x| {
        rl.drawPixel(@intCast(x), y, .dark_gray);
        if (D > 0) {
            y = y + yi;
            D = D + (2 * (dy - dx));
        } else {
            D = D + 2 * dy;
        }
    }
}

fn plotLineHigh(x0: i32, y0: i32, x1: i32, y1: i32) void {
    var dx: i32 = x1 - x0;
    const dy: i32 = y1 - y0;
    var xi: i32 = 1;

    if (dx < 0) {
        xi = -1;
        dx = -dx;
    }

    var D: i32 = (2 * dx) - dy;
    var x: i32 = x0;

    for (@intCast(y0)..@intCast(y1)) |y| {
        rl.drawPixel(x, @intCast(y), .dark_gray);
        if (D > 0) {
            x = x + xi;
            D = D + (2 * (dx - dy));
        } else {
            D = D + 2 * dx;
        }
    }
}

fn plotLine(x0: i32, y0: i32, x1: i32, y1: i32) void {
    if (@abs(y1 - y0) < @abs(x1 - x0)) {
        if (x0 > x1) {
            plotLineLow(x1, y1, x0, y0);
        } else {
            plotLineLow(x0, y0, x1, y1);
        }
    } else {
        if (y0 > y1) {
            plotLineHigh(x1, y1, x0, y0);
        } else {
            plotLineHigh(x0, y0, x1, y1);
        }
    }
}
