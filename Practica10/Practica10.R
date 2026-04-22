#############################################################################
# Práctica 10: Triangulación Denaulay
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 22/04/2026 
# Propósito: Triangular una nave de star wars siguien el nuevo método
##############################################################################
install.packages("readxl")
install.packages("deldir")
install.packages("tripack")
install.packages("plotrix")

library(deldir)
library(ggplot2)
library(plotrix)
library(readxl) 
###Lectura nave
datos <- read_excel("navestarwars.xlsx")
head(datos)
x <- datos$x
y <- datos$y
plot(datos$x, datos$y)
V <- as.matrix(datos[, c("x", "y")])
V



###Triangulación vieja####
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
resultado<-triangulacion(V)
dibujar_triangulacion(V, resultado)


####Nueva triangulación

x <- datos$x
y <- datos$y
dxy1 <- deldir(x,y,dpl=NULL, rw=NULL, plotit=TRUE)
plot(dxy1)

# centroides de la triangulacion
l<-tile.list(dxy1)
g<-tile.centroids(l)
plot(l,close=TRUE)
points(g,pch=20,col="red")
# Ejemplo con voronoi
set.seed(1)
pts <-cbind(X=rnorm(500,rep(seq(1,9,by=2)/10,100),.022),Y=rnorm(500,.5,.15))
plot(pts)



# Generar algunos puntos de ejemplo
x <- datos$x
y <- datos$y
# Calcular triangulación de Delaunay
d <- deldir(x, y)
# Extraer triangulación
tri <- triang.list(d)
# Mostrar resultados
print(tri)
# Graficar
plot(d)
points(x, y, col = "red", pch = 19)