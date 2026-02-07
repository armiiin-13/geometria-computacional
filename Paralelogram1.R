##################################################################################
# Práctica 1: Paralelogramo
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 05/02/2026 
# Propósito: Calcular el área de un paralelogramo descomponiéndolo en triángulos 
# (usando únicamente dos vectores)
##################################################################################
module <- function(v){
  # Comprobar que el argumento de entrada es un vector
  if (!is.vector(v)){
    stop("El argumento de la función debe ser un vector")
  }
  
  # es lo mismo que hacer sqrt(sum(v^2))
  counter <- 0
  n <- length(v)
  for (i in 1:n){
    counter <- counter + v[i]^2
  }
  
  return(sqrt(counter))
}

dot_product <- function(v1, v2){
  # Ya se ha comprobado si son vectores de la misma longitud
  suma <- 0
  n <- length(v1)
  for (i in 1:n){
    suma <- suma + (v1[i] * v2[i])
  }
  # Código equivalente a sum(v1 * v2)
  return(suma)
}

areaParalelogram <- function(v1, v2){
  # Comprobar que los dos argumentos son vectores de la misma longitud (no nulos)
  if (!is.vector(v1) || !is.vector(v2)){
    stop("Alguno de los argumentos no es un vector")
  }
  
  if (length(v1) != length(v2)){
    stop("Los vectores deben ser de la misma longitud")
  }
  
  if (module(v1) == 0 || module(v2) == 0){
    stop("Alguno de los vectores es nulo")
  }
  
  # Los dos vectores forman un triangulo de área 1/2(|a||b| sen \alpha)
  # Se sabe que el ángulo \alpha se puede obtener como arccos(|a·b|/(|a||b|))
  alpha <- acos(abs(dot_product(v1, v2))/ (module(v1) * module(v2)))
  areaTriangulo <- 0.5 * (module(v1) * module(v2) * sin(alpha))
  
  # El paralelogramo que determinan v1 y v2 son dos triángulos de los anteriores
  # unidos por uno de sus lados
  
  areaParalelogramo <- 2 * areaTriangulo
  return(areaParalelogramo)
}

areaParalelogram(c(1,0), c(0,1))
areaParalelogram(c(2,0), c(0,3))