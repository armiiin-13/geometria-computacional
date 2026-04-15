#############################################################################
# Práctica 7: Algoritmos de Envolvente Convexa
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 11/04/2026 
# Propósito: implementar el algoritmo de Sean de Graham
##############################################################################

# Exercise 1 (OPTIONAL)
coordenadas_bar <- function(A,B,C,D){
  #compruebo si hay puntos repetidos
  if( all(A==B) || all(A==C) || all(B==C) ){
    stop("A, B y C deben ser distintos")
  }
  # veo que no sean colineales
  det <- (B[1]-A[1])*(C[2]-A[2]) -
    (B[2]-A[2])*(C[1]-A[1])
  
  if(det == 0){
    stop("A, B y C son colineales")
  }
  # sistema baricentrico
  M <- matrix(c(
    A[1], B[1], C[1],
    A[2], B[2], C[2],
    1,    1,    1
  ), nrow=3, byrow=TRUE)  #matriz del sistema de ecuaciones variables lamda 
  
  b <- c(D[1], D[2], 1) #solución buscada 
  
  lambda <- solve(M,b) #resolvemos el sistema
  
  la <- lambda[1]
  lb <- lambda[2]
  lc <- lambda[3]
  #uso cat para imprimir para poder imprimir el resultado de la variable de forma continua
  cat("lambda_A =", la,"\n")
  cat("lambda_B =", lb,"\n")
  cat("lambda_C =", lc,"\n")
  #veamos si la combinación es convexa
  convexa <- (la >= 0 && lb >= 0 && lc >= 0)
  if(convexa){
    cat("Combinacion convexa\n")
  } else {
    cat("Combinacion NO convexa\n")
  }
  
  #bordes del plot
  xs <- c(A[1],B[1],C[1],D[1])
  ys <- c(A[2],B[2],C[2],D[2])
  #poligonal ABC usando plot
  x <- c(A[1], B[1], C[1], A[1])
  y <- c(A[2], B[2], C[2], A[2])
  plot(x, y, type="l", lwd=2, asp=1,
       xlim=range(xs), ylim=range(ys),
       xlab="X", ylab="Y",
       main="Poligonal ABC y punto D")
  #puntos
  points(A[1],A[2],pch=19)
  points(B[1],B[2],pch=19)
  points(C[1],C[2],pch=19)
  points(D[1],D[2],col="red",pch=19)
  #etiquetas
  text(A[1],A[2],"A",pos=3)
  text(B[1],B[2],"B",pos=3)
  text(C[1],C[2],"C",pos=3)
  text(D[1],D[2],"D",pos=3)
}
#ejemplo
A <- c(0,0)
B <- c(2,0)
C <- c(4,2)
D <- c(2,1)
coordenadas_bar(A,B,C,D)

# Exercise 2
right_turn_on_line <- function(p1, p2, p3){
  # Notilla de Armin: voy a suponer para el algoritmo que si hace un giro a la
  # derecha la función devuelve TRUE y si no FALSE. Si quieres devolver otra
  # cosa me dices para cambiar mi implementación (btw digo lo mismo de los parámetros)
  
  if (length(p1) != 2 || length(p2) != 2 || length(p3) != 2) {
    stop("Cada punto debe tener exactamente dos coordenadas: c(x, y).")
  }
  # por si hay recta vertical
  if (B[1] == A[1]) {
    return(C[1] > A[1])
  }
  
  m <- (B[2] - A[2]) / (B[1] - A[1])
  n <- A[2] - m * A[1]
  
  y_recta <- m * C[1] + n
  
  return(C[2] < y_recta)
}

# Exercise 3
right_turn_matrix <- function(p1, p2, p3){
  # Notilla de Armin: voy a suponer para el algoritmo que si hace un giro a la
  # derecha la función devuelve TRUE y si no FALSE. Si quieres devolver otra
  # cosa me dices para cambiar mi implementación. Los parámetros también elijes
  # tú lo que mejor veas, porque para la implementación voy a usar la función
  # anterior :)
  det <- (p2[1] - p1[1]) * (p3[2] - p1[2]) -
    (p2[2] - p1[2]) * (p3[1] - p1[1])
  
  if (det < 0) {
    return(TRUE)   # giro a la derecha
  }
  else {
    return(FALSE)  # izquierda o alineados
  }
}

# Auxiliar Functions
get_tallest_point <- function(points){
  best_point <- points[[1]]
  best_index <- 1
  
  for (i in 1:length(points)) {
    if (points[[i]][2] > best_point[2]){
      best_point <- points[[i]]
      best_index <- i
    } else if (points[[i]][2] == best_point[2]){
      if (points[[i]][1] > best_point[1]){
        best_point <- points[[i]]
        best_index <- i
      }
    }
  }
  
  return(list(point = best_point, index = best_index))
}

order_points <- function(points, referencial_point){
  P_mat <- do.call(rbind, points)
  dx <- P_mat[,1] - referencial_point[1]
  dy <- P_mat[,2] - referencial_point[2]
  
  angles <- atan2(dy, dx)
  angles[angles < 0] <- angles[angles < 0] + 2*pi
  dists <- dx^2 + dy^2
  
  ordered_points <- points[order(angles, dists, decreasing = TRUE)]
  return(ordered_points)
}


# Algorithm
scan_de_graham_algorithm <- function(P){
  referencial_point <- get_tallest_point(P)
  P <- P[-referencial_point$index]
  P_ordered <- order_points(P, referencial_point$point)
  P_ordered <- c(list(referencial_point$point), P_ordered)
  
  n <- length(P_ordered)
  
  if (n <= 3){
    return(P_ordered) # point, line or triangle
  }
  
  i <- 1
  convex_hull <- list()
  
  while (i + 2 <= n){
    if (right_turn_on_line(P_ordered[[i]], P_ordered[[i+1]], P_ordered[[i+2]])){
      # there is a right turn --> add P_i to convex_hull list
      convex_hull <- c(convex_hull, list(P_ordered[[i]]))
      i <- i + 1
    } else {
      # there is a left turn --> remove P_{i+1} of list
      index <- i + 1
      P_ordered <- P_ordered[-index]
      n <- n - 1
      i <- max(1, i - 1)
    }
  }
  
  # last points
  convex_hull[[length(convex_hull) + 1]] <- P_ordered[[n-1]]
  convex_hull[[length(convex_hull) + 1]] <- P_ordered[[n]]
  
  return(convex_hull)
}

# Plot the Convex Hull
plot_convex_hull <- function(P, convex_hull){
  P_mat <- do.call(rbind, P)
  hull_mat <- do.call(rbind, convex_hull)
  
  # draw points
  plot(
    P_mat[,1], P_mat[,2],
    pch = 19,
    xlab = "x",
    ylab = "y",
    main = "Puntos y envolvente convexa",
    asp = 1
  )
  
  # draw convex hull
  if (nrow(hull_mat) >= 2){
    hull_closed <- rbind(hull_mat, hull_mat[1, , drop = FALSE])
    lines(hull_closed[,1], hull_closed[,2], col = "red", lwd = 2)
    points(hull_mat[,1], hull_mat[,2], pch = 19, col = "blue")
  }
}

# Testing 1
P_test1 <- list(
  c(0,0), c(2,1), c(4,0),
  c(4,4), c(2,3), c(0,4),
  c(2,2)
)

convex_hull <- scan_de_graham_algorithm(P_test1)
plot_convex_hull(P_test1, convex_hull)

# Testing 2
P_test2 <- list(
  c(0,0), c(10,0), c(10,10), c(0,10),
  c(6,6), c(4,4), c(7,3),
  c(3,7), c(5,2), c(2,5)
)

convex_hull <- scan_de_graham_algorithm(P_test2)
plot_convex_hull(P_test2, convex_hull)

# Testing 3
P_test3 <- list(
  c(0,0), c(2,0), c(5,0), c(10,0),
  c(10,5), c(10,10), c(5,10), c(2,10), 
  c(0,10), c(0,5), c(5,5)
)

convex_hull <- scan_de_graham_algorithm(P_test3)
plot_convex_hull(P_test3, convex_hull)

# Testing 4
P_test4 <- list(
  c(0,0), c(2,5), c(4,0),
  c(1,3), c(3,3), c(2,2),
  c(2,6), c(5,3), c(-1,3)
)

convex_hull <- scan_de_graham_algorithm(P_test4)
plot_convex_hull(P_test4, convex_hull)

# Testing 5
P_test5 <- list(
  c(10,10), c(6,10), c(3,9), c(1,8),
  c(0,4), c(0,0), c(10,0), c(9,1), 
  c(7,2), c(5,5), c(6,7), c(5,4)
)

convex_hull <- scan_de_graham_algorithm(P_test5)
plot_convex_hull(P_test5, convex_hull)
