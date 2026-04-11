#############################################################################
# Práctica 7: Algoritmos de Envolvente Convexa
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 11/04/2026 
# Propósito: implementar el algoritmo de Sean de Graham
##############################################################################

# Exercise 1 (OPTIONAL)

# Exercise 2
right_turn_on_line <- function(p1, p2, p3){
  # Notilla de Armin: voy a suponer para el algoritmo que si hace un giro a la
  # derecha la función devuelve TRUE y si no FALSE. Si quieres devolver otra
  # cosa me dices para cambiar mi implementación (btw digo lo mismo de los parámetros)
  
  
  if (length(p1) != 2 || length(p2) != 2 || length(p3) != 2) {
    stop("Cada punto debe tener exactamente dos coordenadas: c(x, y).")
  }
  
  cross_prod <- (p2[1] - p1[1]) * (p3[2] - p1[2]) - 
    (p2[2] - p1[2]) * (p3[1] - p1[1])
  
  return(cross_prod <= 0)
}

# Exercise 3
right_turn_matrix <- function(p1, p2, p3){
  # Notilla de Armin: voy a suponer para el algoritmo que si hace un giro a la
  # derecha la función devuelve TRUE y si no FALSE. Si quieres devolver otra
  # cosa me dices para cambiar mi implementación. Los parámetros también elijes
  # tú lo que mejor veas, porque para la implementación voy a usar la función
  # anterior :)
  
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