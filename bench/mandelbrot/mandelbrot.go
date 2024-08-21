package main

import (
	"bytes"
	"fmt"
)

func main() {
	// start := timestamp();
	out := render()
	fmt.Println(out)
	// end := timestamp();
	// @println("Finished running main() in % milliseconds.", end.sub(start));
}

type Pos struct {
	x, y float64
}

func mandelbrot(c, z Pos, steps int) int {
	i := 0
	zSq := z.mul(z)
	for i < steps && zSq.x+zSq.y < 4.0 {
		z.y = z.x * 2.0 * z.y
		z.x = zSq.x - zSq.y
		z = z.add(c)
		zSq = z.mul(z)
		i += 1
	}
	return i
}

func render() string {
	max_steps := 45
	width := 70
	height := 35
	x_speed := 0.03
	y_speed := 0.06
	x_start := 0.0 - 1.5
	y_start := 0.0 - 1.0

	var out bytes.Buffer
	out.Grow(width * height)
	pos := init_(x_start, y_start)

	for h := 0; h < height; h++ {
		for w := 0; w < width; w++ {
			steps := mandelbrot(pos, init_(0.0, 0.0), max_steps)
			if steps == max_steps {
				out.WriteString("@")
			} else {
				out.WriteString(" ")
			}
			pos.x += x_speed
		}

		out.WriteString("|\n")
		pos.x = x_start
		pos.y += y_speed
	}
	out.WriteString("\n")
	return out.String()
}

func init_(x, y float64) Pos {
	return Pos{x: x, y: y}
}

func (a Pos) add(b Pos) Pos {
	return Pos{x: a.x + b.x, y: a.y + b.y}
}
func (a Pos) sub(b Pos) Pos {
	return Pos{x: a.x - b.x, y: a.y - b.y}
}

func (a Pos) mul(b Pos) Pos {
	return Pos{x: a.x * b.x, y: a.y * b.y}
}

func (a Pos) div(b Pos) Pos {
	return Pos{x: a.x / b.x, y: a.y / b.y}
}
