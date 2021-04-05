# Desarrollo del ejercicio de los apuntes sobre un ajuste exponencial:

x <- c(0,1,2,3,6)
y <- c(7,5,4,3.5,3)
N <- 5

# Se nos pide ajustar esos datos a una curva del tipo y = a * e ^ (b*x)
# Ulteriormente se nos pide hallar la varianza residual y el coeficiente de determinación

# y = a * e ^ (b*x)
# ln(y) = ln(a) + b*x
# Aplicamos ln para la a,y pero no para la x,b

# Planteamos las matrices

A <- matrix(c(N,sum(x),sum(x), sum((x)^2)),nrow = 2)
B <- matrix(c(sum(log(y)),sum(x * log(y))))
res <- solve(A,B)

# Extraemos los datos que necesitamos de la resolucion del sistema de matrices.

cat(sprintf("Los valores que buscamos para resolver el sistema son  a =  %f b = %f\n", res[1],res[2]))
cat(sprintf("El ajuste quedaría de la siguinete manera y =  %f * e ^ (%f * x) \n", exp(res[1]),res[2]))


evaluar <- function(vec){
  5.884550*exp(1)^(-0.130719*vec)
} 

y_est = evaluar(x)

VarY <- mean(y ^2) - mean(y)^2 
VarRes <- mean((y - y_est)^2) - (mean((y - y_est)))^2

R2 <- 1 - VarRes/VarY
