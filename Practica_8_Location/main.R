#############################################################################
# Práctica 8: Localización
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 26/03/2026 
# Propósito: 
##############################################################################

# Data Structs
coordinates <- function(x, y){
  return (list(x = x, y = y))
}

node <- function(name, coordinates, demand){
  list(
    name = name, 
    coordinates = coordinates, 
    demand = demand, 
    server = NULL, 
    used_capacity = 0, 
    score = 0
  )
}

# Stablish the nodes set from a given file
create_nodes_from_file <- function(path){
  lines <- readLines(path)
  if (length(lines) < 2) {
    stop("Invalid file: empty or incomplete")
  }
  
  split_lines <- strsplit(trimws(lines), "\\s+")
  
  header <- as.numeric(split_lines[[1]])
  if (length(header) < 3) {
    stop("Invalid header format")
  }
  
  n <- header[1]
  p <- header[2]
  capacity <- header[3]
  
  nodes <- list()
  
  for (i in 2:length(split_lines)) {
    values <- as.numeric(split_lines[[i]])
    if (length(values) < 4) next
    
    id <- values[1]
    x <- values[2]
    y <- values[3]
    demand <- values[4]
    
    nodes[[length(nodes) + 1]] <- node(
      name = id,
      coordinates = coordinates(x, y),
      demand = demand
    )
  }
  
  if (length(nodes) != n) {
    warning("Number of parsed nodes does not match header")
  }
  
  return(list(nodes = nodes, p = p, capacity = capacity))
}

# Greedy algorithm auxiliar functions
check_feasibility <- function(nodes, p, capacity){
  global_demand <- 0
  
  for (node in nodes){
    if (node$demand > capacity){
      stop("There are nodes with demand greater than server capacity")
    }
    global_demand <- global_demand + node$demand
  }
  
  if (global_demand > p * capacity){
    stop("Global demand exceeds total available capacity")
  }
  
  return(TRUE)
}

euclidean_distance <- function(p1, p2){
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

calculate_scores <- function(candidates){
  for (i in seq_along(candidates)) {
    score <- 0
    
    for (j in seq_along(candidates)) {
      if (i != j) {
        score <- score + candidates[[j]]$demand * euclidean_distance(
          candidates[[i]]$coordinates,
          candidates[[j]]$coordinates
        )
      }
    }
    
    candidates[[i]]$score <- score
  }
  
  return(candidates)
}

get_potential_server <- function(candidates){
  candidates <- calculate_scores(candidates)
  scores <- sapply(candidates, function(node) node$score)
  best_idx <- which.min(scores)
  return(candidates[[best_idx]])
}

get_client_queue <- function(server, candidates){
  if (length(candidates) == 0) {
    return(list())
  }
  
  distances <- sapply(candidates, function(node) {
    euclidean_distance(server$coordinates, node$coordinates)
  })
  
  ordered_idx <- order(distances)
  return(candidates[ordered_idx])
}

remove_node_by_name <- function(nodes, name) {
  Filter(function(nd) nd$name != name, nodes)
}

find_node_index_by_name <- function(nodes, name) {
  for (i in seq_along(nodes)) {
    if (nodes[[i]]$name == name) {
      return(i)
    }
  }
  return(NA_integer_)
}

find_node_by_name <- function(nodes, name) {
  for (nd in nodes) {
    if (nd$name == name) {
      return(nd)
    }
  }
  return(NULL)
}

# Greedy algorithm
greedy_establish_servers <- function(nodes, p, limit_capacity){
  check_feasibility(nodes, p, limit_capacity) # check if the problem can get a solution
  
  candidates <- nodes # R uses copy-on-modified: changing candidates will not change nodes
  servers <- list()
  
  while (length(servers) < p && length(candidates) > 0){
    # Get potential server
    potential_server <- get_potential_server(candidates)
    candidates <- remove_node_by_name(candidates, potential_server$name)
    
    # Server attend itself
    potential_server$used_capacity <- potential_server$demand 
    server_idx <- find_node_index_by_name(nodes, potential_server$name)
    nodes[[server_idx]]$server <- potential_server$name
    
    # Assign nearest clients while capacity allows
    client_queue <- get_client_queue(potential_server, candidates)
    i <- 1
    while (i <= length(client_queue)) {
      client <- client_queue[[i]]
      
      if (potential_server$used_capacity + client$demand <= limit_capacity) {
        potential_server$used_capacity <- potential_server$used_capacity + client$demand
        client_idx <- find_node_index_by_name(nodes, client$name)
        nodes[[client_idx]]$server <- potential_server$name
        candidates <- remove_node_by_name(candidates, client$name)
      }
      
      i <- i + 1
    }
    
    # Add server to partial solution
    servers[[length(servers) + 1]] <- potential_server
  }
  
  if (length(servers) == p && length(candidates) == 0){
    return(list(nodes = nodes, servers = servers))
  } else {
    stop("Unreacheable solution with this greedy strategy")
  }
}

# Evaluate optimal solution
objective_function <- function(solution){
  total_value <- 0
  
  for (server in solution$servers){
    for (node in solution$nodes){
      if (node$server == server$name){
        total_value <- total_value + node$demand * 
          euclidean_distance(server$coordinates, node$coordinates)
      }
    }
  }
  
  return(total_value)
}

evaluate_solution_quality <- function(solution, best_value){
  data_objective_function <- objective_function(solution)
  min_data <- min(data_objective_function)
  gap <- (min_data - best_value) / best_value * 100
  gap
}

# Graphics
plot_solution_connections <- function(solution, alpha) {
  nodes <- solution$nodes
  servers <- solution$servers
  
  xs <- sapply(nodes, function(node) node$coordinates$x)
  ys <- sapply(nodes, function(node) node$coordinates$y)
  
  plot(
    xs, ys,
    xlim = c(min(xs) - alpha, max(xs) + alpha),
    ylim = c(min(ys) - alpha, max(ys) + alpha),
    pch = 16,
    col = "black",
    xlab = "X",
    ylab = "Y",
    main = "P-mediana: conexiones cliente-servidor"
  )
  
  # Draw client-server connections
  for (nd in nodes) {
    if (!is.null(nd$server)) {
      server_node <- find_node_by_name(nodes, nd$server)
      
      if (!is.null(server_node)) {
        segments(
          x0 = nd$coordinates$x,
          y0 = nd$coordinates$y,
          x1 = server_node$coordinates$x,
          y1 = server_node$coordinates$y,
          col = "gray"
        )
      }
    }
  }
  
  # draw clients
  points(xs, ys, pch = 16, col = "black")
  
  # draw servers
  server_x <- sapply(servers, function(s) s$coordinates$x)
  server_y <- sapply(servers, function(s) s$coordinates$y)
  
  points(server_x, server_y, pch = 15, cex = 1.5, col = "red")
  
  # labels
  text(xs, ys, labels = sapply(nodes, function(node) node$name), pos = 3, cex = 0.8)
}

# Testing 1
data <- create_nodes_from_file("Practica_8_Location/phub_50_5_1.txt")
solution <- greedy_establish_servers(data$nodes, data$p, data$capacity)
evaluate_solution_quality(solution,713)
plot_solution_connections(solution,10)

# Testing 2
data <- create_nodes_from_file("Practica_8_Location/phub_50_5_2.txt")
solution <- greedy_establish_servers(data$nodes, data$p, data$capacity)
plot_solution_connections(solution,8)

# Testing 3
data <- create_nodes_from_file("Practica_8_Location/phub_50_5_3.txt")
solution <- greedy_establish_servers(data$nodes, data$p, data$capacity)
plot_solution_connections(solution,7)

# Testing 4
data <- create_nodes_from_file("Practica_8_Location/phub_50_5_4.txt")
solution <- greedy_establish_servers(data$nodes, data$p, data$capacity)
plot_solution_connections(solution,7)

# Testing 5
data <- create_nodes_from_file("Practica_8_Location/phub_50_5_5.txt")
solution <- greedy_establish_servers(data$nodes, data$p, data$capacity)
plot_solution_connections(solution,10)