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

are_colineal <- function(v1, v2){
  if (v1[1] != 0 && v2[1] == 0){
    return(FALSE)
  }
  if (v1[1] == 0 && v2[1] == 0){
    if (length(v1) > 1){
      return(are_colineal(v1[-1], v2[-1]))
    } else {
      return(TRUE) # v1 y v2 son el vector nulo
    }
  }
  lambda <- v1[1]/v2[1]
  n = length(v1)
  for (i in 2:n){
    if (v1[i] != lambda * v2[i]){
      return(FALSE)
    }
  }
  return(TRUE)
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

area_paralelogram <- function(v1, v2){
  # Se comprueba que los dos argumentos son vectores de la misma longitud (no nulos)
  if (!is.vector(v1) || !is.vector(v2)){
    stop("Alguno de los argumentos no es un vector")
  }
  
  # Se comprueba que los vectores tienen la misma dimensión
  if (length(v1) != length(v2)){
    stop("Los vectores deben ser de la misma longitud")
  }
  
  # Se comprueba que los vectores no son nulos
  if (module(v1) == 0 || module(v2) == 0){
    stop("Alguno de los vectores es nulo")
  }
  
  # Se comprueba que los vectores no son colineales
  if (are_colineal(v1,v2)){
    stop("Los vectores son colineales")
  }
  
  # Los dos vectores forman un triangulo de área 1/2(|a||b| sen \alpha)
  # Se sabe que el ángulo \alpha se puede obtener como arccos(|a·b|/(|a||b|))
  alpha <- acos(abs(dot_product(v1, v2))/ (module(v1) * module(v2)))
  area_triangulo <- 0.5 * (module(v1) * module(v2) * sin(alpha))
  
  # El paralelogramo que determinan v1 y v2 son dos triángulos de los anteriores
  # unidos por uno de sus lados
  
  area_paralelogramo <- 2 * area_triangulo
  return(area_paralelogramo)
}

distance <- function(p1,p2){
  difference <- p2-p1
  n <- length(difference)
  sum <- 0
  for (i in 1:n){
    sum <- sum + difference[i]^2
  }
  return(sum)
}

get_triangles <- function(p1,p2,p3,p4){
  points <- list(p1,p2,p3,p4)
  
  # Encontrar los puntos que conforman la diagonal (los más alejados entre si)
  best_i <- 1
  best_j <- 2
  best_distance <- distance(p1,p2)
  
  for (i in 1:3){
    for (j in (i+1):4){
      value <- distance(points[[i]], points[[j]])
      if (value > best_distance){
        best_i <- i
        best_j <- j
        best_distance <- value
      }
    }
  }
  
  # best_i y best_j son los puntos de la diagonal
  others <- setdiff(1:4, c(best_i, best_j))
  
  triangle_1 <- cbind(points[[best_i]], points[[others[[1]]]], points[[best_j]])
  triangle_2 <- cbind(points[[best_i]], points[[others[[2]]]], points[[best_j]])
  
  return(list(t1 = triangle_1, t2 = triangle_2))
}

draw_triangle <- function(t, n){
  x <- c(t[1, ], t[1, 1])
  y <- c(t[2, ], t[2, 1])
  if (n == 1){ # Primer triángulo
    polygon(x, y, col='lightgoldenrod', border='navy', lwd=2)
  } else if (n == 2){ # Segundo Triángulo
    polygon(x, y, col='palegreen', border='navy', lwd=2)
  }
}

draw_paralelogram <- function(p, v1,v2){
  # p es el punto de origen de los dos vectores que componen el paralelogramo
  
  # Se comprueba que los vectores y el punto están en la misma dimensión
  if (!(length(v1) == length(v2) && length(v2) == length(p))){
    stop("Algún elemento no está en la dimensión correcta")
  }
  
  # Se comprueba que los vectores sean vectores
  if (!is.vector(v1) || !is.vector(v2)){
    stop("Alguno de los argumentos no es un vector")
  }
  
  # Se comprueba que los vectores no sean nulos
  if (module(v1) == 0 || module(v2) == 0){
    stop("Alguno de los vectores es nulo")
  }
  
  # Se comprueba que los vectores no son colineales
  if (are_colineal(v1,v2)){
    stop("Los vectores son colineales")
  }
  
  # Hallar los 4 vértices (p es uno de ellos)
  p1 = p + v1
  p2 = p + v2
  p3 = p + v1 + v2

  # Se obtiene los vectores de las x y de las y (para definir los límites del gráfico)
  matrix <- cbind(p, p1, p2, p3)
  x_vector <- matrix[1, ]
  y_vector <- matrix[2, ]
  
  # Se genera el plano del gráfico
  plot(0,0,type="n", xlim = c(min(x_vector) -1, max(x_vector)+1), ylim = c(min(y_vector) -1, max(y_vector)+1), main = paste("Paralelogramo"))
  
  # Se obtienen los dos triangulos que conforman el paralelogramo
  triangles <- get_triangles(p,p1,p2,p3)
  
  # Dibujar el paralelogramo (los dos triángulos)
  draw_triangle(triangles$t1,1)
  draw_triangle(triangles$t2,2)
}

# Testing 
v1 = c(3,2)
v2 = c(0,0) # vector nulo
v3 = c(-1, -5)
v4 = c(9,1)
v5 = c(-2,4)
v6 = c(6,4) # colineal con v1
p_origin = c(0,0)
p = c(3,2)

## Areas
area_paralelogram(v3,v2)
area_paralelogram(v1,v6)
area_paralelogram(v4,v5)
area_paralelogram(v1,v3)

## Drawings
draw_paralelogram(p_origin,v3,v2)
draw_paralelogram(p_origin,v1,v6)
draw_paralelogram(p_origin, v4, v5)
draw_paralelogram(p,v1,v3)
