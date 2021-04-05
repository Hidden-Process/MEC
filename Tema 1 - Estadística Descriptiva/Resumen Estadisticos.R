# Composicion de Vectores:

x <- c(1, 2)
y <- c(3, 6, 7, 11)
z <- c(x, y)

# Subsetting:

z <- 11:20
z[c(3, 4, 6)]
z[c(T, T, F, F, F, F, T, T, T, T)]

# Definición de Listas:

ll <- list(2, 3, "vc", "dfs", T, F)

# Esto es una sublista

ll[1]

# Esto extrae un valor de una lista

ll[[1]]

# Definición de matrices

A <- matrix(c(1, 2, 5, -1, 2, 11, 2, 3, 0), ncol=3)
B <- matrix(c(1, 2, 5, -1, 2, 11, 2, 3, 0), ncol=3, byrow=T)

# Acceso a un elemento

A[2,3]

# Acceso como vector

A[4]

# Producto como vectores

A * B

# Producto matricial

A %*% B

# Solución de sistema de ecuaciones lineal

b <- c(1, -1, 2)
solve(A, b)

# Resolver una ecuación matricial

M <- matrix(c(1, 12, 5, -21, 21, 1, -2, 3, 15), ncol=3)
solve(A, M)

# Inversa de una matriz

solve(A)

# Operaciones básicas

length(ap)
sum(ap)
mean(ap)
sort(ap)
min(ap)
max(ap)

# Media Aritmetica:

valores <- c(7,11,11,8,12,7,6,6)
mean(valores)

# Media ponderada:

notas <- c(2.6,3.7,5.1,4.9,6.4)
ponderacion <- c(1,1,1,2,3)

sum(notas*ponderacion) / sum(ponderacion)

# Media cuadratica

llamadas <- c(2,3,1,0,4,3)
sqrt((sum(llamadas^2)) / length(llamadas))


# Moda:

sort(table(vec), decreasing = TRUE)

# Mediana

median(ap)

# Cuantiles

# Si no es un número entero exacto o x.5 se puede redondear hacia arriba o abajo.

ListaC <- c(2,5,3,4,7,0,11,2,3,8)
quantile(ListaC)

# Percentil 37 y 68

quantile(ListaC,.37)
quantile(ListaC,.68)

# Rango Intercuartilico (Q3 - Q1):

(quantile(ListaC,.75) - quantile(ListaC,.25))

# Desviacion media y Error Cuadratico Medio

ListaD <- c(5,2,3,3,3,5,7)

mean(ListaD)
median(ListaD)

# Respecto a la media

DM <- mean(abs(ListaD - mean(ListaD)))
ECM <- mean((ListaD - mean(ListaD))^2)

# Respecto a la mediana

DM1 <- mean(abs(ListaD - median(ListaD)))
ECM1 <- mean((ListaD - median(ListaD))^2)

# Varianza y desviación típica o estandar.

# No confundir con cuasivarianza (var) o cuasidesviacion tipica(sd) que se usan con muestras para estimar en poblaciones completas

Lista1 <- c(12,10,9,9,10)
Lista2 <- c(5,10,16,15,4)


Var1 <- mean(Lista1 ^2) - mean(Lista1)^2 
Var2 <- mean(Lista2 ^2) - mean(Lista2)^2 

Desv1 <- sqrt(Var1)
Desv2 <- sqrt(Var2)

# Más formas de calcular la varianza:

mean((ap - mean(ap))^2)
mean(ap^2) - mean(ap)^2
sum((ap - mean(ap))^2)/length(ap)
var(ap) * (length(ap)-1) / length(ap)

# Desviación típica

sqrt(mean(ap^2) - mean(ap)^2)
sd(ap) * sqrt((length(ap)-1) / length(ap))

# Cuasivarianza

sum((ap - mean(ap))^2) / (length(ap)-1)
var(ap)

# Variable tipificada

hh <- rnorm(100,175,10)
desviacion <- sqrt(mean((hh - mean(hh))^2))
zi <- (hh - mean(hh))/desviacion

# Momentos de orden 1:

mOrd <- function(x,r) mean(x^r)
mCen <-function(x,r) mean((x - mean(x))^r)

# Momentos de orden 2:


Mord2 <- function(x, y, r, s) mean(x ^ r * y ^ s)
Mcentr2 <- function(x, y, r, s) mean((x - mean(x)) ^ r * (y - mean(y)) ^ s)


# Cálculo de la media usando momento ordinario

mOrd(hh,1)

# Calculo de la varianza (Es mejor usar la segunda opción, los momentos ordinales son mas eficientes computacionalmente)

mCen(hh,2)
mOrd(hh,2) - mOrd(hh,1)^2

# Coeficiente de asimetria de Fisher -> Simetria

mCen(hh,3)/mCen(hh,2)^(3/2)
mCen(hh,3) / (sqrt(mean((hh - mean(hh))^2)))^3

library(e1071)
skewness(densidad,type = 1)

# Coeficiente de Apuntalamiento de Fisher / Curtosis -> Aplastamiento

mCen(hh,4)/mCen(hh,2)^2 - 3
(mCen(hh,4) / (mean(hh ^2) - mean(hh)^2)^2) -3

library(e1071)
kurtosis(densidad,type = 1)

# Histograma

hist(ap)

# Uso de NA

sum(xx, na.rm=T)
mean(xx, na.rm=T)

# Indentificación y eliminación de NAs

is.na(xx)
xx[!is.na(xx)]

# Funciones sample y table (Para generacion de numeros aleatorios y contar frecuencias respectivamente)

xx <- sample(1:10, 100, replace=T)
table(xx)

# Covarianza

x <- ap
y <- 1:144
mean(x * y) - mean(x) * mean(y)
cov(x, y) * (length(ap)-1) / length(ap)

Mcentr2(x, y, 1, 1)
Mord2(x, y, 1, 1) - mean(x) * mean(y)

#  Trabajo con intervalos:

seq(0, 700, 50)
findInterval(ap, seq(0, 700, 50))

# Tabla de frecuencias de intervalos

50*(findInterval(ap, seq(0, 700, 50))-1)
table(50*(findInterval(ap, seq(0, 700, 50))-1))


# Ejercicio 1: (Incompleto)

int <- c(400,700,900,1100,1300,1550)
ni <- c(4,31,136,165,67,14)

# La función rep nos repite el primer valor que le pasemos, el número de veces que le pasemos como segundo argumento.
vector <- rep(int,ni)
hist(vector)

# Ejercicio 2: (Correcto) 

# xi real y estimacion con las frecuencias por  separado
# Calculo de su desviacion típica y ECM

xi_estimado <- c(0,0,0,1,1,1,3,3,3)
xi_real <- c(0,1,3,0,1,3,0,1,3)
ni <- c(6,3,2,3,5,1,1,3,7)

xi <- abs(xi_estimado - xi_real)

MediaCuadratica <- sqrt(sum((xi^2)*ni) / sum(ni))

DM <- sum(ni*xi)/sum(ni)
ECM <- sum(ni*(xi)^2)/sum(ni)

# Ejercicio 3: Momento de orden 1 y 2 respecto a la mediana
 
data <- c(0,0,2,2,2,2,3,3,4,4,7,7)
mean(data - median(data))
mean((data - median(data))^2)


