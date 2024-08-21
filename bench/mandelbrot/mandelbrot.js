function main() {
  const start = performance.now();
  const out = render();
  const end = performance.now();
  console.log(out);
  console.log(
    "Finished running main() in " + Math.round(end - start) + " milliseconds.",
  );
}

function render() {
  const max_steps = 45;
  const width = 70;
  const height = 35;
  const x_speed = 0.03;
  const y_speed = 0.06;
  let x_start = 0.0 - 1.5;
  let y_start = 0.0 - 1.0;

  let out = "";
  const pos = new Pos(x_start, y_start);

  for (let h = 0; h < height; h++) {
    for (let w = 0; w < width; w++) {
      const steps = mandelbrot(pos, new Pos(0.0, 0.0), max_steps);
      if (steps == max_steps) {
        out += "@";
      } else {
        out += " ";
      }
      pos.x += x_speed;
    }

    out += "|\n";
    pos.x = x_start;
    pos.y += y_speed;
  }
  out += "\n";
  return out;
}

function mandelbrot(c, z, steps) {
  let i = 0;
  let zSq = z.mul(z);
  while (i < steps && zSq.x + zSq.y < 4.0) {
    z.y = z.x * 2.0 * z.y;
    z.x = zSq.x - zSq.y;
    z = z.add(c);
    zSq = z.mul(z);
    i += 1;
  }
  return i;
}

class Pos {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  add(b) {
    return new Pos(this.x + b.x, this.y + b.y);
  }

  sub(b) {
    return new Pos(this.x - b.x, this.y - b.y);
  }

  mul(b) {
    return new Pos(this.x * b.x, this.y * b.y);
  }

  div(b) {
    return new Pos(this.x / b.x, this.y / b.y);
  }
}

main();
