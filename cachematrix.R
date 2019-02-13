## The aim of functions defined with this script is to chace the inverse of a matrix, taking advantage
## of the scoping rules of R, so that, if inverse matrix was computed before, there's no need to
## compute it again


## makeCacheMatrix() initialize the cached matrix and define a list of methods for setting and accessing
## cached matrix and it's inverse
##
## Methods are stored into a named list for an easy access with $
##
## Function doesn't compute inverse matrix
##
## When a new cached matrix is inizialized, directly as a formal argument or with $set(), inverse matrix
## is set to NULL

makeCacheMatrix <- function(input_matrix = matrix()) {
      
      inverse_matrix <- NULL
      
      set <- function(matrix_to_set) {
            input_matrix <<- matrix_to_set
            inverse_matrix <<- NULL
      }
      
      get <- function() input_matrix
      
      set_inverse <- function(inverse) inverse_matrix <<- inverse
      
      get_inverse <- function() inverse_matrix
      
      list(set = set,
           get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## cacheSolve() takes an object (a matrix) created using makeCacheMatrix(), directly or via $set(),
## and check if inverse matrix was already computed
##
## -- if inverse matrix was already computed the function return the value stored with set_inverse() method
## of makeCachematrix() and message "getting cached data"
##
## -- if inverse matrix was never computed function does the computation and store it with set_inverse()

cacheSolve <- function(input_matrix, ...) {
      
      inverse_matrix <- input_matrix$get_inverse()
      
      if(!is.null(inverse_matrix)) {
            message("getting cached data")
            return(inverse_matrix)
      }
      
      data <- input_matrix$get()
      
      inverse_matrix <- solve(data, ...)
      
      input_matrix$set_inverse(inverse_matrix)
      
      inverse_matrix
}
