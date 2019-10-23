# Examination Advanced R Programming
# Problem 1 ----
# a. Provide the names of R's object oriented systems. Very briey write how they differ between each other.
"S3: Simple, methods belong to functions.
 S4: More formal, includes fields and parents as well.
 RC: More recent, no copy-on-modify, methods belong to objects, objects have fields and methods"

# b. The behaviour of many functions, e.g. print() or plot() is different when applied to different types of input. 
#    How is this achieved?
"By overloading generic methods for input paramters of a certain type. Fore example, for a created class. In S4, 
this can be done with the SetMethod function"

# Problem 2 ----
# a.
book <-setRefClass("book",
                   fields = list(
                     book_id = "numeric",
                     book_title = "character",
                     book_author = "character",
                     available = "logical"
                   ),
                   methods = list(
                     initialize = function(id, title, author) {
                       stopifnot(is.numeric(id), is.character(c(title, author)))
                       book_id <<- id
                       book_title <<- title
                       book_author <<- author
                       available <<- TRUE
                     }
                   ))

build_library <- setRefClass("build_library",
                             fields = list(
                               library_capacity = "numeric",
                               number_of_books = "numeric",
                               number_of_loaned = "numeric",
                               books = "list"
                             ),
                             methods = list(
                               initialize = function(book_capacity) {
                                 stopifnot(is.numeric(book_capacity))
                                 library_capacity <<- book_capacity
                                 number_of_books <<- 0
                                 number_of_loaned <<- 0
                               },
                               acquire_book = function(title, author) {
                                 stopifnot(is.character(c(title, author)))
                                 if (number_of_books < library_capacity) {
                                   books <<- append(books, book(length(books) + 1, author, title))
                                   number_of_books <<- number_of_books + 1
                                 }
                                 else {
                                   print("capacity is full")
                                 }
                               },
                               borrow_book = function(id) {
                                 stopifnot(is.numeric(id))
                                 if (books[[id]]$available == TRUE) {
                                   books[[id]]$available <<- FALSE
                                   number_of_loaned <<- number_of_loaned + 1
                                 }
                                 else {
                                   print("book not available")
                                 }
                               },
                               return_book = function(id) {
                                 stopifnot(is.numeric(id))
                                 if (books[[id]]$available == FALSE) {
                                   books[[id]]$available <<- TRUE
                                   number_of_loaned <<- number_of_loaned - 1
                                 }
                                 else {
                                   print("book already in system")
                                 }
                               }
                             ))

my_library <- build_library(book_capacity=100)
my_library

# b.
for (i in 1:120) {
  my_library$acquire_book("title","author")
}

my_library$library_capacity
my_library$number_of_books
my_library$number_of_loaned
my_library$books[1]
my_library$books[50]
my_library$books[100]
my_library$books[101]

# c.
for (i in 1:20) {
  my_library$borrow_book(i)
  if (i > 4) {
    my_library$return_book(i - 4)
  }
}

my_library$books[1]
my_library$books[15]
my_library$books[16]
my_library$books[17]
my_library$books[18]
my_library$books[20]              

# Problem 3 ----
# a.
quad_form <- function(aMatrix, aVector) {
  stopifnot(is.numeric(c(aMatrix, aVector)), is.matrix(aMatrix))
  stopifnot(nrow(aMatrix) == ncol(aMatrix), nrow(aMatrix) == length(aVector))
  p <- nrow(aMatrix)
  quad <- 0
  for (i in 1:p) {
    for (j in 1:p) {
      quad <- quad + aVector[i] * aMatrix[i, j] * aVector[j]
    }
  }
  return(as.matrix(quad))
}

#b.
"O(n^2)"

#c.
library(testthat)
myMatrix <- matrix(c(1:25), nrow = 5)                             
myVector <- c(21:25)                           
x <- quad_form(myMatrix, myVector) 
expect_equal(x, myVector%*%myMatrix%*%myVector)

