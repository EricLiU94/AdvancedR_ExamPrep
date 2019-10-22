sheldon_game <- function(player1, player2) {
  alt <- c("rock", "lizard", "spock", "scissors", "paper")
  stopifnot(player1 %in% alt, player2 %in% alt)
  alt1 <- which(alt %in% player1)
  alt2 <- which(alt %in% player2)
  
  if (alt1 == alt2) {
    return("Draw!")
  }
  else if (any(alt1 - alt2 == c(-1, -3, 5, 2))) {
    return("Player 1 wins!")
  }
  else {
    return("Player 2 wins!")
  }
}

my_moving_median <- function(x, n, ...) {
  stopifnot(is.vector(x), is.numeric(n))
  myMedian <- vector()
  for (i in 1:(length(x) - n)) {
    myMedian[i] <- median(x[i:(i + n)], ...)
  }
  return(myMedian)
}

for_mult_table <- function(from, to) {
  stopifnot(is.numeric(from), is.numeric(to))
  myMatrix <- matrix(nrow = (to - from + 1), ncol = (to - from + 1),
                     dimnames = list(c(from:to), c(from:to)))
  for (i in from:to) {
    for (j in from:to) {
      myMatrix[(i - from + 1), j - from + 1] <- i * j
    }
  }
  return(myMatrix)
}

cor_matrix <- function(X) {
  stopifnot(is.data.frame(X))
  numVars <- ncol(X)
  
  meanValues <- sapply(1:numVars, function(currentVar) mean(X[,currentVar]))
  errorValues <- X
  sqErrValues <- X
  for (i in 1:numVars) {
    errorValues[,i] <- errorValues[,i] - meanValues[i]
    sqErrValues[,i] <- errorValues[,i]^2
  }
  stdValues <- sapply(1:numVars, function(currentVar) sqrt(sum(sqErrValues[,currentVar]) / (numVars - 1)))
  
  myMatrix <- matrix(nrow = numVars, ncol = numVars)
  for (i in 1:numVars) {
    for (j in 1:numVars) {
      myMatrix[i, j] <- ((sum(errorValues[,i] * errorValues[,j]) / (stdValues[i] * stdValues[j])) / (numVars - 1))
    }
  }
  return(myMatrix)
}

find_cumsum <- function(x, find_sum) {
  stopifnot(is.vector(is.numeric(x)), is.numeric(find_sum))
  mySum <- 0
  i <- 1
  while (mySum < find_sum && i <= length(x)) {
    mySum <- mySum + x[i]
    i <- i + 1
  }
  return(mySum)
}

while_mult_table <- function(from, to) {
  stopifnot(is.numeric(from), is.numeric(to))
  myMatrix <- matrix(nrow = (to - from + 1), ncol = (to - from + 1),
                     dimnames = list(c(from:to), c(from:to)))
  i <- from
  j <- from
  while (i <= to) {
    while (j <= to) {
      myMatrix[(i - from + 1), j - from + 1] <- i * j
      j <- j + 1
    }
    i <- i + 1
    j <- from
  }
  return(myMatrix)
}

trial_division_factorization <- function(x) {
  as.integer(x)
  a <- vector()
  while (x %% 2 == 0) {
    a <- append(a, 2)
    x <- x / 2
  }
  f = 3
  while (f * f <= x) {
    if (x %% f == 0) {
      a <- append(a, f)
      x <- x / f
    }
    else {
      f <- f + 2  
    }
  }
  if (x != 1) {
    a <- append(a, x)
  }
  return(a)
}

repeat_find_cumsum <- function(x, find_sum) {
  stopifnot(is.vector(is.numeric(x)), is.numeric(find_sum))
  mySum <- 0
  i <- 1
  repeat{
    mySum <- mySum + x[i]
    i <- i + 1
    if (mySum >= find_sum || i > length(x)) {
      break
    }
  }
  return(mySum)
}

repeat_my_moving_median <- function(x, n, ...) {
  stopifnot(is.vector(x), is.numeric(n))
  myMedian <- vector()
  i <- 1
  repeat {
    myMedian[i] <- median(x[i:(i + n)], ...)
    i <- i + 1
    if (i > length(x) - n) {
      break
    }
  }
  return(myMedian)
}

in_environment <- function(env) {
  return(ls(env))
}

where <- function(fun) {
  stopifnot(is.character(fun))
  found <- FALSE
  skip  <- vector()
  for (i in 1:length(search())) {
    if (length(ls(search()[i])) == 0) {
      skipvector <- append(skip, i)
    }
  }
  for (i in 1:length(search())) {
    if (found == TRUE) {
      break
    }
    else if (i %in% skipvector == FALSE) {
      for (j in 1:length(ls(search()[i]))) {
        if (ls(search()[i])[j] == fun) {
          found <- TRUE
          break
        }
      }
    }
  }
  if (found == TRUE) {
    return(search()[i])
  }
  else {
    print(paste(fun, "not found!"))
  }
}

cov <- function(X) {
  stopifnot(is.data.frame(X))
  myVector <- unlist(lapply(1:ncol(X), function(i) sd(X[,i]) / mean(X[,i])))
  names(myVector) <- c(colnames(X))
  return(myVector)
}

moment <- function(i) {
  stopifnot(is.numeric(i))
  myFunction <- function(X) {
    moment <- 0
    for (j in 0:i) {
      moment <- moment + choose(i, j) * (-1)^(i-j) * mean(X^j) * mean(X)^(i-j)
    }
    return(moment)
  }
  return(myFunction)
}

mcmc_counter_factory <- function(burnin, thin) {
  iteration    <<- 0
  store_sample <<- FALSE
  samples      <<- 0
  
  myFunction <- function() {
    iteration <<- iteration + 1
    if ((iteration - burnin) %% thin == 0 & iteration >= burnin + thin) {
      store_sample <<- TRUE
      samples      <<- samples + 1
    }
    else {
      store_sample <<- FALSE
    }
    return(list(iteration, store_sample, samples))
  }
  return(myFunction)
}
