# -----------------------------------------
# Ejemplo 1:
#------------------------------------------

xx <- c(2.67,3.20,4.87,4.62,5.90,5.52,5.11,4.95,4.66,6.25)

# 4 Primeros términos sucesion medias moviles de orden 3:

# Forma manual:

sum(xx[1:3])/3
sum(xx[2:4])/3
sum(xx[3:5])/3
sum(xx[4:6])/3

# Forma Automática:

library(zoo)
data <- rollmean(xx,3)
data[1:4]

# 4 primeros terminos sucesion medias moviles de orden 4:

# Forma manual:

(xx[1]*.5 + xx[2] + xx[3] + xx[4] + xx[5]*.5) / 4
(xx[2]*.5 + xx[3] + xx[4] + xx[5] + xx[6]*.5) / 4
(xx[3]*.5 + xx[4] + xx[5] + xx[6] + xx[7]*.5) / 4
(xx[4]*.5 + xx[5] + xx[6] + xx[7] + xx[8]*.5) / 4

# Forma Automática.

data2 <- rollmean(rollmean(xx,4),2)
data2[1:4]

# -----------------------------------------
# Ejemplo 2:
#------------------------------------------

# Datos
yy <- c(6,6.2,5.1,4.9,5.2,5.8,7)

# Visualización
entrada <- matrix(yy, ncol=7)
colnames(entrada) <- c(1959:1965)

# Tendencia usando medias moviles de orden 3 y 4
datayy1 <- rollmean(yy,3)
datayy2 <- rollmean(rollmean(yy,4),2)