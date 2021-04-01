E <- c(10,19,29,40,48,56)
C <- c(seq(2,12,2))

# Ajustar modelo lineal que pase por el origen, es decir sin término independiente
# Ajustar Modelo E = a * C
# Conseguimos la expresion matricial realizando la derivada parcial respecto a que minimice los errores de la función.
# sum(e*c) = a * sum(c^2) y utilizamos la funcion solve para resolver dicha ecuación.

A <- matrix(sum(C^2))
B <- matrix(sum(E*C))

a <- solve(A,B)

E_est = a * C
