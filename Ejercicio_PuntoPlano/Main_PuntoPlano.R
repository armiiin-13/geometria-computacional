##################################################################################
# Ejercicio 2: Posición relativa punto-plano
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 15/02/2026 
# Propósito: Establecer si un punto está por encima, por debajo o dentro de un plano dado
##################################################################################
create_plane <- function(A,B,C,D){
  if (A == 0 && B == 0 && C==0){
    stop("Un plano no puede tener asociado un vector normal nulo")
  }
  return(list(A=A, B=B, C=C, D=D))
}

evaluate <- function(P, plane){
  if (length(P) != 3){
    stop("El punto no pertenece a R3")
  }
  return(plane$A*P[1] + plane$B*P[2] + plane$C*P[3] + plane$D)
}

relative_position <- function(P, plane){
  eval <- evaluate(P, plane)
  if (eval > 0){
    print("P está por encima del plano")
  } else if (eval < 0){
    print("P está por debajo del plano")
  } else {
    print("P está dentro del plano")
  }
}

# Testing
plane_1 = create_plane(2,3,4,1)
plane_2 = create_plane(5,-2,-1,6)
plane_3 = create_plane(1,1,0,0)
P = c(0,1,-1) # in 1
Q = c(-1,0,1) # in 2
R = c(1,-1,6) # in 3
P_inv = c(0,-1,1)
Q_inv = c(1,0,-1)
R_inv = c(-1,-1,-6)

## Testing -> case in
relative_position(P, plane_1)
relative_position(Q, plane_2)
relative_position(R, plane_3)

## Testing -> case above
relative_position(P, plane_3)
relative_position(Q, plane_1)
relative_position(R, plane_2)

## Testing -> case below
relative_position(P_inv, plane_3)
relative_position(Q_inv, plane_1)
relative_position(R_inv, plane_2)

## Testing -> errors
plane_error <- create_plane(0,0,0,7)
relative_position(c(1,1,1,1), plane_1)