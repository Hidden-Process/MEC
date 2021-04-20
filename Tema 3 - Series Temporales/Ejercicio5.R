# Datos: Evolución del precio del azucar en esos años.
precio <- c(25,29,34,38,42,45,70,77)
tiempo <- c(1975:1982)

# Función para calcular la relación de precios tomando el primer dato como referencia, correspondiente
# a 1975 (Esto tambien se conoce como números índices)

func <- function(vector){
  for(i in 1:length(vector)){
    vec2 <- vector[i] / vector[1]
    cat(sprintf("El valor del indice %i es %#.2f, lo que en porcentaje equivale a  %#.2f \n",i,vec2,(vec2-1)*100))
  }
}

func(precio)

# Vemos los valores indices indican un porcentaje de variación creciente constante que aumenta en mayor 
# proporción cada año que pasa, lo que quiere decir que los precios han subido todos los años, aunque ese
# incremento es especialmente considerable en los últimos años llegando a subir un 180 o 208% en los 2
# ultimos años observados respecto al período de referencia.