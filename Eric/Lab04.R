linreg<- setRefClass("linreg",
                     fields = list(
                       input                    = "character",
                       regression_coef          = "matrix",
                       fitted_values            = "matrix",
                       residuals_values         = "matrix",
                       degrees_of_freedom       = "numeric",
                       residual_variance        = "matrix",
                       n_observation            = "numeric",
                       p_parameters             = "numeric",
                       variance_regression_coef = "matrix",
                       t_values_coef            = "matrix"),
                     methods = list(
                       initialize = function(formula, data) {
                         input <<- paste("linreg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")",
                                         sep = "")
                         X <- model.matrix(formula, data)
                         y <- data.matrix(data[all.vars(formula)[1]])
                         regression_coef <<- solve(t(X) %*% X) %*% t(X) %*% y
                         fitted_values <<- X %*% regression_coef
                         residuals_values <<- y - fitted_values
                         n_observation <<- nrow(data)
                         p_parameters <<- length(all.vars(formula)) - 1
                         degrees_of_freedom <<- n_observation - p_parameters
                         residual_variance <<- (t(residuals_values) %*% residuals_values) / degrees_of_freedom
                         variance_regression_coef <<- as.numeric(residual_variance) * solve(t(X) %*% X)
                         t_values_coef <<- regression_coef / sapply(diag(variance_regression_coef), sqrt)
                         },
                       
                       print = function() {
                         base::cat("Call:\n")
                         base::cat(input, "\n\n")
                         base::cat("Coefficients:\n")
                         myCoef <- regression_coef
                         colnames(myCoef) <- c("")
                         base::print(t(myCoef))
                       }
                       )
                     )
