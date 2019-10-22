euclidean <- function(a, b) {
  stopifnot(is.numeric(c(a, b)))
  while (b != 0) {
    t <- b 
    b <- a %% b 
    a <- t 
  }
  return(a)
}

dijkstra <- function(graph, init_node) {
  stopifnot(is.data.frame(graph), colnames(graph) == c("v1", "v2", "w"), is.numeric(init_node))
  lowestNode <- min(min(graph$v1), min(graph$v2))
  highestNode <- max(max(graph$v1), max(graph$v2))
  
  length <- matrix(nrow = highestNode, ncol = highestNode,
                           dimnames = list(c(lowestNode:highestNode), c(lowestNode:highestNode)))
  for (i in 1:nrow(graph)) {
    length[graph$v1[i], graph$v2[i]] <- graph$w[i]
  }
  length[is.na(length)] <- Inf 
  
  dist <- vector(length = highestNode - lowestNode + 1)
  prev <- vector(length = highestNode - lowestNode + 1)
  Q <- vector()
  
  for (v in lowestNode:highestNode) {
    dist[v] <- Inf
    prev[v] <- NA
    Q <- append(Q, v)
  }
  dist[init_node] <- 0
  
  while (length(Q) > 0) {
    closest <- Inf
    for (i in 1:length(Q)) {
      if (dist[Q[i]] < closest) {
        u <- Q[i]
        closest <- dist[Q[i]]
      }
    }
    Q <- Q[!Q %in% u]
    
    for (v in lowestNode:highestNode) {
      alt <- dist[u] + length[u, v]
      if (alt < dist[v]) {
        dist[v] <- alt
        prev[v] <- u
      }
    }
  }
  return(dist)
}
