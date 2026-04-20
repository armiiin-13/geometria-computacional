#############################################################################
# Práctica 9: Algoritmos de Triangulación
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 11/04/2026 
# Propósito: Conseguir tringular un polígono de n vértices
##############################################################################


orientacion <- function(a, b, c) {
  return((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]))
}
#función que verifica si un punto p está dentro del triángulo abc
punto_en_triangulo <- function(a, b, c, p) {
  o1 <- orientacion(a, b, p)
  o2 <- orientacion(b, c, p)
  o3 <- orientacion(c, a, p)
  
  return((o1 >= 0 && o2 >= 0 && o3 >= 0) || 
           (o1 <= 0 && o2 <= 0 && o3 <= 0))
}
#función que comprueba si tenemos una oreja
es_oreja <- function(V, i) {
  n <- nrow(V)
  
  prev <- V[(i - 2) %% n + 1, ]
  curr <- V[i, ]
  sig  <- V[i %% n + 1, ]
  #Debe ser convexo
  if (orientacion(prev, curr, sig) <= 0) return(FALSE)
  #Ningún otro punto dentro del triángulo
  for (j in 1:n) {
    if (j == i || j == ((i - 2) %% n + 1) || j == (i %% n + 1)) next
    if (punto_en_triangulo(prev, curr, sig, V[j, ])) return(FALSE)
  }
  
  return(TRUE)
}

#Algoritmo principal
triangulacion <- function(V) {
  V <- as.matrix(V)
  triangulos <- list()
  while (nrow(V) > 3) {
    n <- nrow(V)
    encontrado <- FALSE
    for (i in 1:n) {
      if (es_oreja(V, i)) {
        prev_i <- (i - 2) %% n + 1
        next_i <- i %% n + 1
         triangulos[[length(triangulos) + 1]] <- 
          rbind(V[prev_i, ], V[i, ], V[next_i, ])
        
        V <- V[-i, , drop = FALSE]
        encontrado <- TRUE
        break
      }
    }
    
    if (!encontrado) {
      stop("El polígono no es simple o hay error numérico")
    }
  }
  
  triangulos[[length(triangulos) + 1]] <- V
  return(triangulos)
}

#Vamos a plasmar la triangulación resultante del algoritmo
dibujar_triangulacion <- function(V, triangulos) {
  # Dibujar polígono original
  plot(V, type = "n", asp = 1, xlab = "", ylab = "", main = "Triangulación")
  polygon(V, border = "black", col = rgb(0.9, 0.9, 0.9, 0.3))
  colores <- rainbow(length(triangulos))
  for (i in seq_along(triangulos)) {
    tri <- triangulos[[i]]
    polygon(tri, col = adjustcolor(colores[i], alpha.f = 0.4), border = "blue")
    #los bordes
    lines(rbind(tri, tri[1, ]), col = "blue", lwd = 2)
  }
  #Los vertices
  points(V, pch = 19, col = "red")
  text(V, labels = 1:nrow(V), pos = 3, col = "red")
}




#Ejemplos de distintos polígonos
poligono <- matrix(c(
  0, 0,
  1,1,
  2, 0,
  3, 1,
  2, 2,
  0, 2,
  0.5,1
), ncol = 2, byrow = TRUE)

resultado <- triangulacion(poligono)
print(resultado)

dibujar_triangulacion(poligono, resultado)


#####

poligono <- matrix(c(
  0, 0,
  3,1,
  2, 2,
  0, 2,
  0.5,1
), ncol = 2, byrow = TRUE)

resultado <- triangulacion(poligono)
print(resultado)

dibujar_triangulacion(poligono, resultado)
#####

poligono<- matrix(c(
  0,0,
  2,1,
  4,0,
  3,2,
  4,4,
  2,3,
  0,4,
  1,2
), ncol=2, byrow=TRUE)



resultado <- triangulacion(poligono)
print(resultado)

dibujar_triangulacion(poligono, resultado)

#####
poligono <- matrix(c(
  0, 0,
  2, 0.5,
  3, 2,
  2, 3,
  1, 2.5,
  -0.5, 1
), ncol = 2, byrow = TRUE)


resultado <- triangulacion(poligono)
print(resultado)

dibujar_triangulacion(poligono, resultado)
######

poligono <- matrix(c(
  0, 0,
  2, 1,
  1.5, 1,
  1.5, 3,
  0.5, 3,
  0.5, 1,
  0, 1
), ncol = 2, byrow = TRUE)

resultado <- triangulacion(poligono)
print(resultado)

dibujar_triangulacion(poligono, resultado)

####
poligono <- matrix(c(
  0, 0,    
  10, 0,   
  9, 2,    
  8.2, 2,  
  7, 2,    
  6.2, 1, 
  5, 1, 
  4.2, 2,  
  3, 2,  
  2.2, 1,  
  1, 1,   
  0, 2
), byrow = TRUE, ncol = 2)

resultado <- triangulacion(poligono)
print(resultado)

dibujar_triangulacion(poligono, resultado)