x <- c(1:5)
y <- c(1,0.5,0.33,0.25,0.20)

N <- 5

# Ejemplo de ajuste hiperbólico.
# y = 1 / (a + b*x)
# 1/y = a + b*x
# Introducimos el inverso del y en lugar del propio y en nuestro modelo lineal basico:

A <- matrix(c(N,sum(x),sum(x), sum(x^2)),nrow = 2)
B <- matrix(c(sum(1/y),sum(x*(1/y))))

res <- solve(A,B)

cat(sprintf("Los valores que buscamos para el ajuste se corresponden con a =  %f b = %f\n", res[1],res[2]))
cat(sprintf("El ajuste quedaría de la siguinete manera y = 1 / %f + %f x\n", res[1],res[2]))
