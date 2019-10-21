my_num_vector <- function() {
  c(log10(11), cos(pi/5), exp(pi/3), (1173 %% 7) / 19)
}

filter_my_vector <- function(x, leq) {
  sapply(x, function(number) ifelse(number <= leq, NA, number))
}

dot_prod <- function(a, b) {
  sum(a * b)
}

approx_e <- function(N) {
  sum(sapply(c(0:N), function(x) 1/factorial(x)))
}

my_magic_matrix <- function() {
  matrix(c(4, 9, 2,
           3, 5, 7,
           8, 1, 6), nrow = 3, byrow = TRUE)
}

calculate_elements <- function(A) {
  nrow(A) * ncol(A)
}

row_to_zero <- function(A, i) {
  A[i,] <- 0
  return(A)
}

add_elements_to_matrix <- function(A, x, i, j) {
  A[i, j] <- A[i, j] + x
  return(A)
}

my_magic_list <- function() {
  list(info = "my own list",
         my_num_vector(),
         my_magic_matrix())
}

change_info <- function(x, text) {
  x$info <- text
  return(x)
}

add_note <- function(x, note) {
  x$note <- note
  return(x)
}

sum_numeric_parts <- function(x) {
  sum(sapply(x, function(element) ifelse(is.numeric(element), sum(element), 0)))
}

my_data.frame <- function() {
  data.frame(id = c(1, 2, 3), name = c("John", "Lisa", "Azra"), income = c(7.30, 0.00, 15.21), rich = c(FALSE, FALSE, TRUE))
}

sort_head <- function(df, var.name, n) {
  head(df[order(-df[var.name]),])
}

add_median_variable <- function(df, j) {
  df$compared_to_median <- ifelse(df[,j] > median(df[,j]), "Greater",
                                  ifelse(df[,j] < median(df[,j]), "Smaller", "Median"))
  return(df)
}

analyze_columns <- function(df, j) {
  myList <- list(c(mean = mean(df[,j[1]]), median = median(df[,j[1]]), sd = sd(df[,j[1]])),
               c(mean = mean(df[,j[2]]), median = median(df[,j[2]]), sd = sd(df[,j[2]])),
               correlation_matrix = cor(df[j]))
  names(myList)[1:2] <- colnames(df[j])
  return(myList)
}