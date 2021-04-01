x <- c(1:5)
y <- c(0.5,2,4.5,8,12.5)

N <- 5

# Ejemplo de ajuste exponencial.
# y = a * x^b
# ln(y) = ln(a) + b * ln(x)

# Tenemos que aplicar ln a y, a, x , pero no a b

# Convertimos la lineal original, aplicando los algoritmos necesarios.

A <- matrix(c(N,sum(log(x)),sum(log(x)), sum(log(x)^2)),nrow = 2)
B <- matrix(c(sum(log(y)),sum(log(x)*log(y))))

res <- solve(A,B)

cat(sprintf("Los valores que buscamos para resolver el sistema son  a =  %f b = %f\n", res[1],res[2]))
cat(sprintf("El ajuste quedaría de la siguinete manera y =  %f * x ^ %f \n", exp(res[1]),res[2]))
