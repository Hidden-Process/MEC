# Desarrollo del ejercicio de los apuntes sobre un ajuste hiperbólico:

x <- c(0,1,2,3,6)
y <- c(7,5,4,3.5,3)
N <- 5

# Se nos pide ajustar esos datos a una curva del tipo y = 1/a+bx
# Ulteriormente se nos pide hallar la varianza residual y el coeficiente de determinación y ver que ajuste
# es mejor para ese set de datos, el exponencial del script anterior o el hiperbólico del actual.

# y = 1/a+bx
# 1/y = a+bx
# Introducimos el inverso del y en lugar del propio y en nuestro modelo lineal basico:

A <- matrix(c(N,sum(x),sum(x), sum(x^2)),nrow = 2)
B <- matrix(c(sum(1/y),sum(x*(1/y))))

res <- solve(A,B)


cat(sprintf("Los valores que buscamos para el ajuste se corresponden con a =  %f b = %f\n", res[1],res[2]))
cat(sprintf("El ajuste quedaría de la siguinete manera y = 1 / %f + %f x\n", res[1],res[2]))

evaluar <- function(vec){
  1/(0.168958 + 0.030593*vec)
} 

y_est = evaluar(x)

VarY <- mean(y ^2) - mean(y)^2 
VarRes <- mean((y - y_est)^2) - (mean((y - y_est)))^2

R2 <- 1 - VarRes/VarY

# Comprobación del modelo lineal:

model <- lm(y~x)
r_squa <- summary(model)$r.squared

#Vemos como el coeficiente de determinación R² del ajuste hiperbólico es 0.86, el del exponencial 0.81,
# y el del lineal 0.73, por tanto  podemos concluir que el ajuste hiperbólico es el más apropiado 
# en este caso.
