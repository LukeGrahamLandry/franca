//! Note that to be fair you want to time both javac building the class file and the jvm running it.

package bench.mandelbrot;

class mandelbrot {

    public static void main(String[] args) {
        var start = System.currentTimeMillis();
        var out = render();
        var end = System.currentTimeMillis();
        System.out.println(out);
        System.out.println(
            "Finished running main() in " + (end - start) + " milliseconds."
        );
    }

    private static String render() {
        var max_steps = 45;
        var width = 70;
        var height = 35;
        var x_speed = 0.03;
        var y_speed = 0.06;
        var x_start = 0.0 - 1.5;
        var y_start = 0.0 - 1.0;

        var out = new StringBuilder(width * height);
        var pos = new Pos(x_start, y_start);

        for (var h = 0; h < height; h++) {
            for (var w = 0; w < width; w++) {
                var steps = mandelbrot(pos, new Pos(0.0, 0.0), max_steps);
                if (steps == max_steps) {
                    out.append("@");
                } else {
                    out.append(" ");
                }
                pos.x += x_speed;
            }

            out.append("|\n");
            pos.x = x_start;
            pos.y += y_speed;
        }
        out.append("\n");
        return out.toString();
    }

    private static int mandelbrot(Pos c, Pos z, int steps) {
        var i = 0;
        var zSq = z.mul(z);
        while (i < steps && zSq.x + zSq.y < 4.0) {
            z.y = z.x * 2.0 * z.y;
            z.x = zSq.x - zSq.y;
            z = z.add(c);
            zSq = z.mul(z);
            i += 1;
        }
        return i;
    }

    private static class Pos {

        double x;
        double y;

        public Pos(double x, double y) {
            this.x = x;
            this.y = y;
        }

        public Pos add(Pos b) {
            return new Pos(this.x + b.x, this.y + b.y);
        }

        public Pos sub(Pos b) {
            return new Pos(this.x - b.x, this.y - b.y);
        }

        public Pos mul(Pos b) {
            return new Pos(this.x * b.x, this.y * b.y);
        }

        public Pos div(Pos b) {
            return new Pos(this.x / b.x, this.y / b.y);
        }
    }
}
