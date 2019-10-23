# Examination Advanced R Programming
# Problem 1 ----
# a. What is the difference between <- and <<-?
"<-: assigns to the local environment
 <<-: assigns to the parent or global environment"
# b. If you don’t supply an explicit environment, where do ls() and rm() look?
".GlobalEnv" 
"in words, the global environment" 
# c. What is the difference between the fields Depends and Imports in the DESCRIPTION file in an R package? 
"Depends: states what the packages requires to be able to function. For example a certain version of a package or R
 Imports: list libraries that are required for a package to work. These are loaded upon loading the package"
# What needs to be provided in the importFrom() statements in the NAMESPACE file?
"Here a class and a method should be provided, the specific method of this class is loaded"

# Problem 2 ----
cargo <- setRefClass("cargo",
                     fields = list(
                       item_name = "character",
                       item_weight = "numeric"
                     ),
                     methods = list(
                       initialize = function() {
                         item_name <<- character()
                         item_weight <<- numeric()
                       },
                       add_cargo = function(name, weight) {
                         stopifnot(is.character(name), is.numeric(weight))
                         item_name <<- append(item_name, name)
                         item_weight <<- append(item_weight, weight)
                       },
                       remove_cargo = function() {
                         stopifnot(length(item_name) > 1)
                         item_name <<- item_name[2:length(item_name)]
                         item_weight <<- item_weight[2:length(item_weight)]
                       }
                     ))

build_Batory <- setRefClass("build_Batory",
                            fields = list(
                              cargo_weight = "numeric",
                              fuel_weight = "numeric",
                              max_weight = "numeric",
                              cargo = "cargo"
                            ),
                            methods = list(
                              initialize = function(fuel, max_cargo_weight) {
                                stopifnot(is.numeric(c(fuel, max_cargo_weight)))
                                cargo_weight <<- fuel
                                max_weight <<- max_cargo_weight
                                cargo <<- cargo()
                                fuel_weight <<- fuel
                              },
                              equip_Batory = function(name, weight) {
                                stopifnot(is.character(name), is.numeric(weight))
                                if ((cargo_weight + weight < max_weight)) {
                                  cargo$add_cargo(name, weight)
                                  cargo_weight <<- cargo_weight + weight
                                }
                                else {
                                  base::print(paste(name, "does no fit on the ship anymore"))
                                }
                              },
                              embark_sailor = function(weight) {
                                stopifnot(is.numeric(weight))
                                if (weight < max_weight) {
                                  while(cargo_weight + weight > max_weight) {
                                    if (cargo$item_name[1] != "sailor") {
                                      cargo$remove_cargo()
                                      cargo_weight <<- sum(cargo$item_weight) + fuel_weight
                                    }
                                    else {
                                      print("There are no more items to remove, except for sailors")
                                    }
                                  }
                                  equip_Batory("sailor", weight)
                                }
                                else {
                                  base::print("The sailor is too heavy for the ship")
                                }
                              },
                              print = function() {
                                cat(paste("The ORP Batory with a maximum cargo weight of:", max_weight), "\n")
                                cat(paste("Has equipped a total weight of: ", round(cargo_weight, 3)), "\n")
                                cat("Including:", "\n")
                                cat(paste(fuel_weight, "in weight of fuel"), "\n")
                                cat(paste(round(sum(sapply(1:length(cargo$item_name), function(i) 
                                  ifelse(cargo$item_name[i] != "sailor", cargo$item_weight[i], 0))),3),
                                  "in weight of cargo"), "\n")
                                cat(paste(round(sum(sapply(1:length(cargo$item_name), function(i)
                                  ifelse(cargo$item_name[i] == "sailor", cargo$item_weight[i], 0))),3),
                                  "in weight of sailors"), "\n")
                              },
                              plot = function() {
                                data <- data.frame(fuel = fuel_weight,
                                                   items = round(sum(sapply(1:length(cargo$item_name), function(i) 
                                                     ifelse(cargo$item_name[i] != "sailor", cargo$item_weight[i], 0))),3),
                                                   sailors =round(sum(sapply(1:length(cargo$item_name), function(i)
                                                     ifelse(cargo$item_name[i] == "sailor", cargo$item_weight[i], 0))),3))
                                bp <- barplot(as.matrix(data), col = "blue", main = "Weight division")
                                text(bp, 0, data, pos = 3)
                              }
                            ))

# a.
ORP_Batory <- build_Batory(fuel=1.5,max_cargo_weight=10)
ORP_Batory

# b.
for (i in 1:50) {
  ORP_Batory$equip_Batory("food", rexp(n = 1, rate = 0.5))  
}

ORP_Batory

# c.
for (i in 1:10) {
  ORP_Batory$embark_sailor(runif(n = 1, min = 0.08, max = 0.2))
}

ORP_Batory

# d.
ORP_Batory$print()
ORP_Batory$plot()

# Problem 3
# a.
matrix_mult <- function(A, B) {
  stopifnot(is.numeric(c(A, B)), ncol(A) == nrow(B))
  n <- nrow(A)
  m <- ncol(B)
  p <- ncol(A)
  V <- matrix(0, nrow = n, ncol = m)
  
  for (i in 1:n) {
    for (j in 1:m) {
      for (k in 1:p) {
        V[i, j] <- V[i, j] + A[i, k] * B[k, j]
      }
    }
  }
  return(V)
}

# b. 
"O(n^3)"

# c.
library(testthat)
A <- matrix(c(1:10), nrow = 2)
B <- matrix(c(11:20), ncol = 2)
V <- matrix_mult(A, B)
expect_equal(V, A %*% B)


