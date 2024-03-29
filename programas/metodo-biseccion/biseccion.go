package main

import (
	"fmt"
	"math"
)

func main() {
	n := 13.0
	tol := 0.5 * math.Pow(10, 2 - n)
	root, eps_a := bisect(f, 0, 10, tol, 0)
	fmt.Println(root, "\t", eps_a)
}

func bisect(f func(x float64) float64, low, up, tol, depth float64) (float64, float64) {
	middle := (low + up) / 2.0

	eps_a := math.Abs((low - middle) / middle) * 100

	if depth > 100 || eps_a < tol {
		return middle, eps_a
	}

	if f(low) * f(middle) < 0 {
		return bisect(f, low, middle, tol, depth + 1)
	} else {
		return bisect(f, middle, up, tol, depth + 1)
	}
}

func f(x float64) float64 {
	return x*x - 4 - math.Sin(x)
}
