# Simple Pendulum Problem
using OrdinaryDiffEq, Plots

#Constants
const g = 9.81
L = 1.0

#Initial Conditions
u0_small = [π/20, 0]
u0_big = [π/4, 0]
tspan = (0.0, 10)

#Define the problem
function simplependulum(du, u, p, t)
    θ = u[1]
    dθ = u[2]
    du[1] = dθ
    du[2] = -(g/L)*sin(θ)
end

#Define the problem
function simplelinearpendulum(du, u, p, t)
    θ = u[1]
    dθ = u[2]
    du[1] = dθ
    du[2] = -(g/L)*θ
end

println("hola")

#Pass to solvers
small_problem = ODEProblem(simplependulum, u0_small, tspan)
small_solution = solve(small_problem,Tsit5())

small_linear_problem = ODEProblem(simplelinearpendulum, u0_small, tspan)
small_linear_solution = solve(small_linear_problem,Tsit5())

big_problem = ODEProblem(simplependulum, u0_big, tspan)
big_solution = solve(big_problem,Tsit5())

big_linear_problem = ODEProblem(simplelinearpendulum, u0_big, tspan)
big_linear_solution = solve(big_linear_problem,Tsit5())

small_plot = plot(small_solution, vars=(0,1), linewidth=2, title ="Péndulo simple, con ángulo pequeño", xaxis = "t", yaxis = "\\theta (t)", label = "No lineal")
small_plot = plot!(small_linear_solution, vars=(0,1), linewidth=2, title ="Péndulo simple, con ángulo pequeño", xaxis = "t", yaxis = "\\theta (t)", label = "Lineal")

savefig(small_plot, "small.png")

big_plot = plot(big_solution, vars=(0,1), linewidth=2, title ="Péndulo simple, con ángulo grande", xaxis = "t", yaxis = "\\theta (t)", label = "No lineal")
big_plot = plot!(big_linear_solution, vars=(0,1), linewidth=2, title ="Péndulo simple, con ángulo grande", xaxis = "t", yaxis = "\\theta (t)", label = "Lineal")

savefig(big_plot, "big.png")
