# Week 3 - R Programming Assignment 2 ----------------

# - The following functions return the inverse matrix
# of a squared invertible matrix passed as argument to
# the functions.
# - Note: if the matrix passed as argument was previously
# passed to the first function (makeCacheMatrix) is 
# the identical matrix passed the argument to second 
# function (cacheSolve), then inverse matrix is retrieved 
# from cache with a message indicating the same.  
# Otherwise, the inverse matrix is calculated and returned.
# - Example of use
#      mat <- makeCacheMatrix(matrix(c(2,2,3,2), 2, 2))
#      cacheSolve(mat)

# makeCacheMatrix: 
# - This function creates a "unique matrix" object
#   that can cache the inverse matrix.
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
#   "unique matrix" returned by makeCacheMatrix above.
# - Argument to function is "unique matrix" returned 
#   by makeCacheMatrix above. 
# - If the inverse has already been calculated for matrix
#   specified in argument, then the cachesolve retrieves 
#   the inverse matrix from the cache.
# - Otherwise, the inverse matrix is calculated.
# - Example of use:   cacheSolve(mat)
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
