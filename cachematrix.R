## Functions - makeCacheMatrix and cacheSolve.
## makeCacheMatrix function creates a matrix and cacheSolve creates the inverse. More details given as comments
## along with the functions below

## makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(set_var) {
    x <<- set_var
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix function.
## If the inverse is already calculated, the inverse should be retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inp_mtrx <- x$get()
  inv <- solve(inp_mtrx, ...)
  x$setInverse(inv)
  inv
}
