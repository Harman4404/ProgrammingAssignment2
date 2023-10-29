## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function serves to create a specialized matrix object with caching capabilities. It does so by encapsulating the matrix data and its cached inverse within a list. This list includes methods for setting and retrieving the matrix data (set and get), as well as for setting and retrieving the cached inverse (setInverse and getInverse). The inv variable is used to store the cached inverse, initially set to NULL.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##The cacheSolve function is designed to compute the inverse of the matrix stored in the special "matrix" object. It checks whether the inverse has already been calculated and cached. If it's found in the cache, the function retrieves the cached inverse, which can save computational time. If the inverse is not cached, the function computes it using the solve function and then stores it in the cache using the setInverse method of the special object created with makeCacheMatrix. The function ultimately returns the inverse matrix, making this pair of functions a convenient tool for efficient matrix inversion with caching capabilities.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
