package main

import (
	"fmt"
	"math"
)

func main() {
	n := 10.0
	tol := 0.5 * math.Pow(10, 2 - n)
	fmt.Println("roots: ", bisect(f, -10, 10, tol, 0))
}

func bisect(f func(x float64) float64, low, up, tol, depth float64) []float64 {
	var roots = []float64{}

	middle := (low + up) / 2.0

	if depth > 1000 || math.Abs(low - middle) < tol {
		return append(roots, middle)
	}

	if f(low) * f(middle) < 0 {
		roots = append(roots, bisect(f, low, middle, tol, depth + 1)...)
	}

	if f(middle) * f(up) < 0 {
		roots = append(roots, bisect(f, middle, up, tol, depth + 1)...)
	}

	return roots
}

func f(x float64) float64 {
	return x*x - 4 - math.Sin(x)
}
