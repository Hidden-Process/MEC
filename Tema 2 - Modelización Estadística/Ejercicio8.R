x <- c(1:5)
y <- c(3,4.5,7,10,15)

N <- 5

# Ejemplo de ajuste parabólico.
# y = a * b^x
# ln(y) = ln(a) + x * ln(b)
# Aplicamos ln a la y, a, b pero no a la x

A <- matrix(c(N,sum(x),sum(x), sum((x)^2)),nrow = 2)
B <- matrix(c(sum(log(y)),sum(x * log(y))))

res <- solve(A,B)

cat(sprintf("Los valores que buscamos para resolver el sistema son  a =  %f b = %f\n", res[1],res[2]))
cat(sprintf("El ajuste quedaría de la siguinete manera y =  %f *  %f^x \n", exp(res[1]),exp(res[2])))
