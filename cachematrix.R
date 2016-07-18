## Keeps in cache the inverse of the matrix, to use it later if it's needed

## The object created is a "special" matrix

makeCacheMatrix2 <- function(z = matrix()) {
  inv <- NULL
  set <- function(y) {
    z <<- y
    inv <<- NULL
  }
  get <- function() z
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'z', if the solution was computed before, it returns the message.

cacheSolve2 <- function(z, ...) {
  inv <- z$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- z$get()
  inv <- solve(mat, ...)
  z$setInverse(inv)
  inv
}
