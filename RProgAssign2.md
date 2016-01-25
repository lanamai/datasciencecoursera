## This is a markdown file

# Week 3 - Programming Assignment 2

# function definitions

# makeCacheMatrix: 
# - This function creates a special "matrix" object
#   that can cache its inverse.
# - Argument to function is an invertible matrix.
# - Example of use
#      mat <- makeCacheMatrix(matrix(c(2,2,3,2), 2, 2))
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# cacheSolve: 
# - This function computes the inverse of the 
#   special "matrix" returned by makeCacheMatrix above. 
# - If the inverse has already been calculated for matrix
#   specified in argument, then the cachesolve retrieves 
#   the inverse from the cache.
# - Argument to function is special "matrix" returned 
#   by makeCacheMatrix above. 
# - Example of use:   
#      cacheSolve(mat)
# - If cacheSolve(mat) run again for unchanged mat, then
#   a message 'getting cached data' precedes inverse matrix.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
