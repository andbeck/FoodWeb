Niche.model <- function(S, L, N=1){
  C <- L/S^2
  if(N==1){
    n <- sort(runif(S))
    beta <- (1 - 2 * C) / (2 * C)
    r <- n*(1 - (1 - runif(S))^(1/beta))
    c <- r/2 + runif(S) * (n - r/2)
    web <- matrix(0,S,S)
    min.n <- c-r/2
    max.n <- c+r/2
    for(i in 1:S){
      diet <- c(1:S)[c(which(n>min.n[i]), which(n<max.n[i]))[duplicated(c(which(n>min.n[i]), which(n<max.n[i])))]]
      web[diet,i] <- 1
    }
    dimnames(web) <- list(1:length(web[,1]), 1:length(web[,1]))
  }
  if(N>1){
    web <- list()
    for(j in 1:N){
      n <- sort(runif(S))
      beta <- (1 - 2 * C) / (2 * C)
      r <- n*(1 - (1 - runif(S))^(1/beta))
      c <- r/2 + runif(S) * (n - r/2)
      web[[j]] <- matrix(0,S,S)
      min.n <- c-r/2
      max.n <- c+r/2
      for(i in 1:S){
        diet <- c(1:S)[c(which(n>min.n[i]), which(n<max.n[i]))[duplicated(c(which(n>min.n[i]), which(n<max.n[i])))]]
        web[[j]][diet,i] <- 1
      }
      dimnames(web[[j]]) <- list(1:length(web[[j]][,1]), 1:length(web[[j]][,1]))
    }
  }
  web
}

# nichesim <- Niche.model(S = vcount(graph), ecount(graph), N = 1)
# nichesim <- graph_from_adjacency_matrix(as.matrix(nichesim))
# plot(nichesim)
