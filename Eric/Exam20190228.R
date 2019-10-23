# Examination Advanced R Programming
# Problem 1 ----
# a. What R package is required to run code in parallel? Provide an example of a function 
#    from that package that is used to run parallel code and discuss its most important parameters.
"library(parallel). An example of a function is parLapply, this is similar to the regular lapply function.
An important addiitional parameter is cl, a cluster object which
provides the different clusters to run parallel code."
# b. What sort of R functions are directly parallelizable, that is with minimum code changes?
"The apply family of functions is directly parallizable. So when these functions are used in non-parallel
code, minimal changes are required."

# Problem 2 ----
# a.
library(ggplot2)
build_data_object <- setRefClass("build_data_object",
                                fields = list(
                                  pValue = "numeric",
                                  cluster_id = "numeric",
                                  cluster_center = "numeric",
                                  cluster_nObservations = "numeric",
                                  observation_id = "numeric",
                                  observation_value = "numeric",
                                  observation_cluster = "numeric",
                                  observation_dCenter = "numeric",
                                  nClusters = "numeric",
                                  nObservations = "numeric"
                                ),
                                methods = list(
                                  initialize = function(p) {
                                    stopifnot(is.numeric(p))
                                    pValue <<- p
                                    nClusters <<- 0
                                    nObservations <<- 0
                                  },
                                  distance = function(x, y) {
                                    stopifnot(is.numeric(c(x, y)))
                                    return(abs(x - y)^pValue)
                                  },
                                  create_cluster = function(center) {
                                    stopifnot(is.numeric(center), (center %in% cluster_center) == FALSE)
                                    cluster_center <<- append(cluster_center, center)
                                    cluster_id <<- append(cluster_id, which(cluster_center %in% center))
                                    nClusters <<- nClusters + 1
                                  },
                                  add_observation = function(observation) {
                                    stopifnot(is.numeric(observation))
                                    observation_value <<- append(observation_value, observation)
                                    observation_id <<- append(observation_id, which(observation_value %in% observation)[1])
                                    myDistances <- sapply(1:nClusters, function(cluster) 
                                      distance(observation, cluster_center[cluster]))
                                    observation_cluster <<- append(observation_cluster,
                                                                   cluster_id[which(myDistances %in% min(myDistances))[1]])
                                    observation_dCenter <<- append(observation_dCenter,
                                                                   myDistances[which(myDistances %in% min(myDistances))[1]])
                                    nObservations <<- nObservations + 1
                                  },
                                  print = function() {
                                    for (i in 1:nClusters) {
                                      cat(paste("Observations in cluster", i, ":", "\n"))
                                      for (j in 1:nObservations) {
                                        if (observation_cluster[j] == i) {
                                          cat(paste(j, " ", sep = ""))
                                        }
                                      }
                                      cat("\n")
                                    }
                                  },
                                  plot = function() {
                                    myData<- data.frame(id = observation_id, 
                                                        value = observation_value, 
                                                        cluster = observation_cluster)
                                    ggplot(myData) +
                                      aes(x = id, y = value, color = cluster) +
                                      geom_point() +
                                      theme_bw()
                                  }
                                ))

data_obj <- build_data_object(p = 2)

# b. 
for (i in 1:10) {
  data_obj$create_cluster(i^2)
}

data_obj

# c. 
for (i in 1:50) {
  data_obj$add_observation(round(runif(n = 1, min = 0, max = 100), 0))
}

data_obj

# d.
data_obj$print()
data_obj$plot()

# Problem 3 ----
# a.
my_binom_coef <- function(n, k) {
  stopifnot(is.numeric(c(n, k)))
  return(factorial(n) / (factorial(n - k) * factorial(k)))
}

my_binom_coef(10, 8)

my_binom_coef2 <- function(n, k) {
  nValue <- 1
  for (i in 1:n) {
    nValue <- nValue * i
    if (i == (n - k)) {
      n_kValue <- nValue
    }
    if (i == k) {
      kValue <- nValue
    }
  }
  return(nValue / (n_kValue * kValue))
}

my_binom_coef2(10, 8)

# b.
"O(1) or O(n)"

# c.
library(testthat)
x <- my_binom_coef(10, 8)
expect_equal(x, choose(10, 8))
y <- my_binom_coef2(10, 8)
expect_equal(y, choose(10,8))
