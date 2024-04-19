//! Renders a 1-bit ascii art mandelbrot set.
//! If you like fractals, check out: https://github.com/LukeGrahamLandry/FractalViewer

#[derive(Clone, Copy)]
struct Pos {
    x: f64,
    y: f64,
}

impl Pos {
    fn add(self, b: Pos) -> Pos {
        Pos {
            x: self.x + b.x,
            y: self.y + b.y,
        }
    }
    fn sub(self, b: Pos) -> Pos {
        Pos {
            x: self.x - b.x,
            y: self.y - b.y,
        }
    }
    fn mul(self, b: Pos) -> Pos {
        Pos {
            x: self.x * b.x,
            y: self.y * b.y,
        }
    }
    fn div(self, b: Pos) -> Pos {
        Pos {
            x: self.x / b.x,
            y: self.y / b.y,
        }
    }
}

fn mandelbrot(c: Pos, mut z: Pos, steps: i64) -> i64 {
    let mut i = 0;
    let mut zSq = z.mul(z);
    while (i < steps && zSq.x + zSq.y < 4.0) {
        z.y = z.x * 2.0 * z.y;
        z.x = zSq.x - zSq.y;
        z = z.add(c);
        zSq = z.mul(z);
        i = i + 1;
    }
    i
}

fn main() {
    let start = timestamp();
    let max_steps = 45;
    let width = 70;
    let height = 35;
    let x_speed = 0.03;
    let y_speed = 0.06;
    let x_start = 0.0 - 1.5;
    let y_start = 0.0 - 1.0;

    let mut out: String = String::with_capacity(width * height);
    let mut pos: Pos = Pos { x: x_start, y: y_start };
    for _ in 0..height {
        for _ in 0..width {
            let steps = mandelbrot(pos, Pos { x: 0.0, y: 0.0 }, max_steps);
            if steps == max_steps {
                out.push('@');
            } else {
                out.push(' ');
            }
            pos.x = pos.x + x_speed;
        }
        out.push('|');
        out.push('\n');
        pos.x = x_start;
        pos.y = pos.y + y_speed;
    }
    println!("{out}");
    let end = timestamp();
    println!("Finished running main() in {:.5} seconds.", end - start);
}

pub fn timestamp() -> f64 {
    use std::time::SystemTime;
    SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64()
}
