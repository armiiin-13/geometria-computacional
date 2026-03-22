##################################################################################
# Ejercicio 5: Transformaciones Afines
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 09/02/2026 
# Propósito: Trabajar con conceptos de geometría afín en R
##################################################################################

# Load packages for 3d graphics
library(rgl)

# Affine Tranformation Functions
traslation <- function(part, q){
  vertex_old <- part$vertex
  vertex_new <- matrix(0, nrow = nrow(vertex_old), ncol = 3)
  
  for (i in 1:nrow(vertex_old)){
    for (j in 1:3){
      vertex_new[i,j] = vertex_old[i,j] + q[j]
    }
  }
  
  part$vertex = vertex_new
  if (part$name == "body"){
    part$extra = q
  }
  return(part)
}

# Simetria respecto a plano suponemos que c=1  z= ax+by +d y movible#################################
symmetry <- function(part, r){
  vertex_old <- part$vertex
  vertex_new <- matrix(0, nrow = nrow(vertex_old), ncol = 3)
  
  d = r(0,0)
  a= r(1,0)-d
  b=r(0,1)-d
  c=-1 #Porque en implicito 0= ax+by-z +d
  vu = c(a,b,c)/sqrt(a^2+b^2+c^2) #normalizo el vector normal
  R = matrix(c(c(1-2*vu[1]^2,-2*vu[2]*vu[1],-2*vu[1]*vu[3],0),c(-vu[1]*2*vu[2],1-2*vu[2]^2,-2*vu[2]*vu[3],0),c(-2*vu[1]*vu[3],-2*vu[2]*vu[3],1-2*vu[3]^2,0),c(0,0,0,1)), nrow=4, ncol=4, byrow=TRUE)
  p0   = c(0,0,r(0,0)) # ax+by+cz+d=0 punto en el plano
  T2 = matrix(c(c(1,0,0,p0[1]),c(0,1,0,p0[2]),c(0,0,1,p0[3]),c(0,0,0,1)), nrow=4, ncol=4, byrow=TRUE)
  T1 = matrix(c(c(1,0,0,-p0[1]),c(0,1,0,-p0[2]),c(0,0,1,-p0[3]),c(0,0,0,1)), nrow=4, ncol=4, byrow=TRUE)
  M = T2%*%R%*%T1
  
  for(i in 1:nrow(vertex_old)){
    p_ = M%*%c(vertex_old[i,1],vertex_old[i,2],vertex_old[i,3],1) #aplico la transformación
    vertex_new[i,]=c(p_[1],p_[2],p_[3])
  }
  part$vertex = vertex_new
  return(part)
}

rotation <- function(part, theta, axis){
  #matriz del movimiento
  
  if(axis!= 1 & axis != 2& axis != 3){
    stop("Eje no válido")
  }
  else{
    if(axis==1){#rotación eje x
      M = matrix(c(
        c(1, 0, 0),
        c(0, cos(theta), -sin(theta)),
        c(0, sin(theta), cos(theta))
      ), nrow=3, ncol=3, byrow=TRUE)
      
    }
    if (axis==2){#eje y
      M= matrix(c(
        c(cos(theta), 0, sin(theta)),
        c(0, 1, 0),
        c(-sin(theta), 0, cos(theta))
      ), nrow=3, ncol=3, byrow=TRUE)
    }
    if (axis==3){#eje z
      
      M = matrix(c(
        c(cos(theta), -sin(theta), 0),
        c(sin(theta), cos(theta), 0),
        c(0, 0, 1)
      ), nrow=3, ncol=3, byrow=TRUE)
    }
    
    vertex_old <- part$vertex
    vertex_new <- matrix(0, nrow = nrow(vertex_old), ncol = 3)
    
    for(i in 1:nrow(vertex_old)){
      vertex_new[i,] = as.vector(M%*%vertex_old[i,])
    }
    part$vertex = vertex_new
    return(part)
  }
}

homothety <- function(part, center, k){
  M = matrix(c(
    c(k, 0, 0, (1 - k)*center[1]),
    c(0, k, 0, (1 - k)*center[2]),
    c(0, 0, k, (1 - k)*center[3]),
    c(0, 0, 0, 1)
  ), nrow = 4, ncol = 4, byrow = TRUE)
  
  vertex_old <- part$vertex
  vertex_new <- matrix(0, nrow = nrow(vertex_old), ncol = 3)
  
  for(i in 1:nrow(vertex_old)){
    p1=c(vertex_old[i,],1)
    p_ = as.vector(M%*%p1)
    vertex_new[i,]= c(p_[1],p_[2],p_[3])
  }
  part$vertex = vertex_new
  return(part)
}

# Porygon-Z is composed by independent parts
create_part <- function(name, vertex, faces){
  list(name = name, vertex = vertex, faces = faces, extra = c(0,0,0))
}

# Draw 3d figures auxiliar functions
draw_part <- function(part, color = "skyblue"){ # every part can be separeted into triangles
  vertex <- part$vertex;
  face <- part$faces;
  
  for(i in seq_len(nrow(face))){
    triangle <- vertex[face[i, ], , drop = FALSE]
    triangles3d(
      x = triangle[,1],
      y = triangle[,2],
      z = triangle[,3],
      color = color
    )
  }
}

draw_body_colored <- function(part, z_blue_min = -0.35, z_blue_max = 0.55) {
  vertex <- part$vertex
  face <- part$faces
  limits <- part$extra
  
  for (i in seq_len(nrow(face))) {
    triangle <- vertex[face[i, ], , drop = FALSE]
    
    z_mean <- mean(triangle[, 3])
    
    color <- if (z_mean >= z_blue_min + limits[3] && z_mean <= z_blue_max + limits[3]) {
      "deepskyblue3"
    } else {
      "hotpink2"
    }
    
    triangles3d(
      x = triangle[,1],
      y = triangle[,2],
      z = triangle[,3],
      color = color
    )
  }
}

draw_model <- function(model){
  open3d(windowRect = c(100, 100, 1200, 900))
  bg3d("white")
  
  if (!is.null(model$body)) draw_body_colored(model$body)
  if (!is.null(model$head)) draw_part(model$head, "hotpink2")
  if (!is.null(model$beak)) draw_part(model$beak, "deepskyblue3")
  if (!is.null(model$antenna)) draw_part(model$antenna, "hotpink2")
  if (!is.null(model$left_arm)) draw_part(model$left_arm, "deepskyblue3")
  if (!is.null(model$right_arm)) draw_part(model$right_arm, "deepskyblue3")
  if (!is.null(model$tail)) draw_part(model$tail, "deepskyblue3")
  
  axes3d()
  grid3d(c("x", "y", "z"))
}

# Auxiliar functions to create the model
make_ring <- function(radius_x, radius_y, z, n_points = 50) {
  angles <- seq(0, 2*pi, length.out = n_points + 1)[- (n_points + 1)]
  x <- radius_x * cos(angles)
  y <- radius_y * sin(angles)
  z <- rep(z, n_points)
  
  cbind(x, y, z)
}

connect_rings <- function(start1, start2, n) {
  faces <- matrix(0, nrow = 2*n, ncol = 3)
  
  for (i in 1:n) {
    next_i <- ifelse(i == n, 1, i + 1)
    
    v1 <- start1 + i - 1
    v2 <- start1 + next_i - 1
    v3 <- start2 + i - 1
    v4 <- start2 + next_i - 1
    
    faces[2*i - 1, ] <- c(v1, v2, v3)
    faces[2*i, ]     <- c(v2, v4, v3)
  }
  
  faces
}

stablish_faces <- function(n_rings, n, top_order, bottom_order){
  idx_ring <- vector("numeric", length = n_rings)

  idx_top <- 1
  idx_ring[1]  <- 2

  for (i in 2:n_rings){
    idx_ring[i] <- idx_ring[i-1] + n
  }

  idx_bottom <- idx_ring[n_rings] + n

  pieces <- list()

  faces_top <- matrix(0, nrow = n, ncol = 3)

  for (i in 1:n) {
    next_i <- ifelse(i == n, 1, i + 1)
    faces_top[i, ] <- make_triangle(
      idx_ring[1] + i - 1,
      idx_ring[1] + next_i - 1,
      idx_top,
      top_order
    )
  }

  pieces <- c(pieces, list(faces_top))

  faces <- vector("list", length = n_rings - 1)

  for (i in 1:(n_rings - 1)) {
    faces[[i]] <- connect_rings(idx_ring[i], idx_ring[i+1], n)
  }

  pieces <- c(pieces, faces)

  faces_bottom <- matrix(0, nrow = n, ncol = 3)

  for (i in 1:n) {
    next_i <- ifelse(i == n, 1, i + 1)

    faces_bottom[i, ] <- make_triangle(
      idx_ring[n_rings] + i - 1,
      idx_ring[n_rings] + next_i - 1,
      idx_bottom,
      bottom_order
    )
  }

  pieces <- c(pieces, list(faces_bottom))
  
  return (do.call(rbind, pieces))
}

make_triangle <- function(a, b, c, order){
  switch(
    order,
    "abc" = c(a, b, c),
    "acb" = c(a, c, b),
    "bac" = c(b, a, c),
    "bca" = c(b, c, a),
    "cab" = c(c, a, b),
    "cba" = c(c, b, a),
    stop("order must be: abc, acb, bac, bca, cab, cba")
  )
}

make_limb <- function(name = "limb", z0 = 0, y_shift = 0, x_shift = 0, n_points = 20, scale_x = 1, scale_y = 1, scale_z = 1) {
  limb_tip <- c(0, 0, 0)
  
  ring1 <- make_ring(0.10 * scale_x, 0.08 * scale_y, 0.18 * scale_z, n_points = n_points)
  ring2 <- make_ring(0.22 * scale_x, 0.16 * scale_y, 0.42 * scale_z, n_points = n_points)
  ring3 <- make_ring(0.16 * scale_x, 0.12 * scale_y, 0.68 * scale_z, n_points = n_points)
  
  limb_end <- c(0, 0, 0.82 * scale_z)
  
  ring1[,1] <- ring1[,1] + x_shift
  ring1[,2] <- ring1[,2] + y_shift
  ring1[,3] <- ring1[,3] + z0
  
  ring2[,1] <- ring2[,1] + x_shift
  ring2[,2] <- ring2[,2] + y_shift
  ring2[,3] <- ring2[,3] + z0
  
  ring3[,1] <- ring3[,1] + x_shift
  ring3[,2] <- ring3[,2] + y_shift
  ring3[,3] <- ring3[,3] + z0
  
  limb_tip <- limb_tip + c(x_shift, y_shift, z0)
  limb_end <- limb_end + c(x_shift, y_shift, z0)
  
  limb_vertex <- rbind(limb_tip, ring1, ring2, ring3, limb_end)
  
  limb_faces <- stablish_faces(n_rings = 3, n = n_points, top_order = "acb", bottom_order = "abc")
  
  create_part(name, limb_vertex, limb_faces)
}

orient_limb_to_x <- function(part) {
  vertex <- part$vertex
  
  vertex <- cbind(vertex[,3], vertex[,2], vertex[,1])
  
  part$vertex <- vertex
  part
}

tilt_limb_down <- function(part, amount = 0.6, side = "right") {
  vertex <- part$vertex
  
  if (side == "right") {
    x0 <- min(vertex[,1])
    vertex[,3] <- vertex[,3] - amount * (vertex[,1] - x0)
  } else {
    x0 <- max(vertex[,1])
    vertex[,3] <- vertex[,3] - amount * (x0 - vertex[,1])
  }
  
  part$vertex <- vertex
  part
}

# Create the base Porygon-Z model
## Body
n_body <- 50

top    <- c(0, 0,  1.2)

ring1  <- make_ring(0.3, 0.55,  0.8, n_points = n_body)
ring2  <- make_ring(0.8, 0.85,  0.2, n_points = n_body)  # blue part
ring3  <- make_ring(0.7, 0.75, -0.5, n_points = n_body)
ring4  <- make_ring(0.2, 0.45, -1.0, n_points = n_body)

bottom <- c(0, 0, -1.4)

body_vertex <- rbind(top, ring1, ring2, ring3, ring4, bottom)

body_faces <- stablish_faces(n_rings = 4, n = n_body, top_order = "cab", bottom_order = "acb")

body <- create_part("body", body_vertex, body_faces)
##################################

## Head
n_head <- 50
z_head <- 2.15

head_top <- c(0, 0,  0.72 + z_head)

head_ring1  <- make_ring(0.52, 0.46,  0.58 + z_head, n_points = n_head)
head_ring2  <- make_ring(0.82, 0.72,  0.22 + z_head, n_points = n_head)
head_ring3  <- make_ring(0.80, 0.70, -0.12 + z_head, n_points = n_head)
head_ring4  <- make_ring(0.50, 0.44, -0.50 + z_head, n_points = n_head)

head_bottom <- c(0, 0, -0.68 + z_head)

head_vertex <- rbind(head_top, head_ring1,head_ring2, head_ring3, head_ring4, head_bottom)

head_faces <- stablish_faces(n_rings = 4, n = n_head, top_order = "cab", bottom_order = "acb")

head <- create_part("head", head_vertex, head_faces)
##################################

## Beak
n_beak <- 30
z_beak <- 2.1

beak_back <- c(0, -0.52, z_beak)

beak_ring1 <- make_ring(0.40, 0.28, -0.80, n_points = n_beak)
beak_ring1 <- beak_ring1[, c(1, 3, 2)]
beak_ring1[,3] <- beak_ring1[,3] + z_beak

beak_ring2 <- make_ring(0.30, 0.22, -1.00, n_points = n_beak)
beak_ring2 <- beak_ring2[, c(1, 3, 2)]
beak_ring2[,3] <- beak_ring2[,3] + z_beak

beak_front <- c(0, -1.5, z_beak)

beak_vertex <- rbind(beak_back, beak_ring1, beak_ring2, beak_front)

beak_faces <- stablish_faces(n_rings = 2, n = n_beak, top_order = "cba", bottom_order = "abc")

beak <- create_part("beak", beak_vertex, beak_faces)
###################################

## Antenna
n_antenna <- 20
s_antenna <- 1.15

antenna_base  <- c(0, 0.00, 2.78)
antenna_ring1 <- make_ring(0.14 * s_antenna, 0.12 * s_antenna, 2.78 * s_antenna, n_points = n_antenna)
antenna_ring2 <- make_ring(0.11 * s_antenna, 0.10 * s_antenna, 2.92 * s_antenna, n_points = n_antenna)
antenna_ring3 <- make_ring(0.08 * s_antenna, 0.07 * s_antenna, 3.04 * s_antenna, n_points = n_antenna)
antenna_ring4 <- make_ring(0.05 * s_antenna, 0.045 * s_antenna, 3.12 * s_antenna, n_points = n_antenna)

antenna_ring2[,2] <- antenna_ring2[,2] + 0.01
antenna_ring3[,2] <- antenna_ring3[,2] + 0.02
antenna_ring4[,2] <- antenna_ring4[,2] + 0.03

antenna_top <- c(0, 0.03 * s_antenna, 3.16 * s_antenna)

antenna_vertex <- rbind(antenna_base, antenna_ring1, antenna_ring2, antenna_ring3, antenna_ring4, antenna_top)

antenna_faces <- stablish_faces(n_rings = 4, n = n_antenna, top_order = "acb", bottom_order = "abc")

antenna <- create_part("antenna", antenna_vertex, antenna_faces)
###################################

## Arms
s_arm <- 1.25
s_tail <- 1.1

right_arm <- make_limb(
  name = "right_arm",
  n_points = 20,
  scale_x = 1.25 * s_arm,
  scale_y = 1.10 * s_arm,
  scale_z = 1.60 * s_arm
)

right_arm <- orient_limb_to_x(right_arm)
right_arm <- tilt_limb_down(right_arm, amount = 0.65, side = "right")

right_arm$vertex[,1] <- right_arm$vertex[,1] + 0.7
right_arm$vertex[,3] <- right_arm$vertex[,3] + 0.1

left_arm <- make_limb(
  name = "left_arm",
  n_points = 20,
  scale_x = 1.25 * s_arm,
  scale_y = 1.10 * s_arm,
  scale_z = 1.60 * s_arm
)

left_arm <- orient_limb_to_x(left_arm)

left_arm$vertex[,1] <- -left_arm$vertex[,1]

left_arm <- tilt_limb_down(left_arm, amount = 0.65, side = "left")

left_arm$vertex[,1] <- left_arm$vertex[,1] - 0.7
left_arm$vertex[,3] <- left_arm$vertex[,3] + 0.1
##################################

## Tail
tail <- make_limb(
  name = "tail",
  x_shift = 0.00,
  y_shift = 0.00,
  z0 = -3,
  n_points = 20,
  scale_x = 1.00 * s_tail,
  scale_y = 0.90 * s_tail,
  scale_z = 1.90 * s_tail
)
##################################

# Stablish the Porygon-Z
porygon_z <- list(
  body = body,
  head = head,
  beak = beak,
  antenna = antenna,
  left_arm = left_arm,
  right_arm = right_arm,
  tail = tail
)

# Draw the Porygon-Z base
draw_model(porygon_z)

# Testing
body = traslation(porygon_z$body, c(20,4,-10))
porygon_z$body = body
draw_model(porygon_z)

head = symmetry(porygon_z$head, function(x,y) 2*x-2*y+4)
porygon_z$head = head
draw_model(porygon_z)

tail = rotation(porygon_z$tail, pi/2, 2)
porygon_z$tail = tail
draw_model(porygon_z)

right_arm = homothety(porygon_z$right_arm, c(1,5,1),4)
porygon_z$right_arm = right_arm
draw_model(porygon_z)