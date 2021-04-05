x <- c(1:5)
y <- c(1,1,2,4,8)
N <- 5

plot(x,y)

# Apartado A:

# A la vista del gráfico de dispersión, no parece que un ajuste lineal sea apropiado, ya que vemos como al 
# principio crece muy despacio, pero conforme la x aumenta sus valores, la y crece mucho más rapido, por lo que
# podemos apreciar del dibujo con estos pocos datos, parece un crecimiento exponencial, por lo que un ajuste de
# ese tipo parece más acertado:

# Apartado B:

# Ajuste de una función:  a * b^x

A <- matrix(c(N,sum(x),sum(x), sum((x)^2)),nrow = 2)
B <- matrix(c(sum(log(y)),sum(x * log(y))))

res <- solve(A,B)

cat(sprintf("Los valores que buscamos para resolver el sistema son  a =  %f b = %f\n", res[1],res[2]))
cat(sprintf("El ajuste quedaría de la siguinete manera y =  %f * %f^x \n", exp(res[1]),exp(res[2])))

# Apartado C:

# Usando Modelo Exponencial:

evaluar <- function(vec){
  exp(res[1])*exp(res[2])^vec
} 

y_est = evaluar(x)

VarY <- mean(y^2) - mean(y)^2 
VarR <- mean((y - y_est)^2) - (mean((y - y_est)))^2
R2_Exp <- 1 - (VarR/VarY)

# Usando modelo Lineal:

model <- lm(y~x)
R2_Lin <-summary(model)$r.squared 

# Comparación de modelos:

cat(sprintf("El coeficiente de determinación del ajuste exponencial es: %f", R2_Exp))
cat(sprintf("El coeficiente de determinación del ajuste lineal es: %f", R2_Lin))
cat(sprintf("Por tanto el mejor ajuste es es el exponencial, ya que explica el  %f por ciento de la varianza de y", (R2_Exp*100)))

# Apartado D:

#Coeficientes del modelo Exponencial.

exp <- list(exp(res[1]),exp(res[2]))

# Coeficientes del modelo Lineal:

lin <- model$coefficients

expfunct <- function(x){
  0.435275 * 1.741101^x
}

linfunct <- function(x){
  -1.9 + 1.7*x
}

# Para x = 6
Exp1 <-  expfunct(6)
Lin1 <- linfunct(6)

# Para x = 10
Exp2 <-  expfunct(10)
Lin2 <- linfunct(10)

cat(sprintf("Ajuste Exponencial para los valores de y cuando x=6 y x=10: =  %f , %f\n", Exp1,Exp2))
cat(sprintf("Ajuste Lineal para los valores de y cuando x=6 y x=10: =  %f , %f\n", Lin1,Lin2))
