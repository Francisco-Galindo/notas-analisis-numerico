package main

import (
	"fmt"
	"math"
)

func main() {
	n := 13.0
	tol := 0.5 * math.Pow(10, 2 - n)
	root, eps_a := bisect(f, -3, -1, 0, tol, 0)
	fmt.Println(root, "\t", eps_a)
}

func bisect(f func(x float64) float64, low, up, prev, tol, depth float64) (float64, float64) {
	middle := up - (f(up) * (low - up)) / (f(low) - f(up))

	eps_a := math.Abs((prev - middle) / middle) * 100

	fmt.Printf("%v\t%v\t%v\n", depth, middle, eps_a)

	if depth > 100 || eps_a < tol {
		return middle, eps_a
	}

	if f(low) * f(middle) < 0 {
		return bisect(f, low, middle, middle, tol, depth + 1)
	} else {
		return bisect(f, middle, up, middle, tol, depth + 1)
	}
}

func f(x float64) float64 {
	return x*x - 4 - math.Sin(x)
}
