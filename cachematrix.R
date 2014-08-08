## Introduces a cached matrix structure, that caches the inverse of the matrix
## Reassigning a value will invalidate the cache and cause a recomputation.

## Makes a list with functions to set data and inverse solution for a cached matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Returns the inverse of a matrix. If it is in the cache it will be used. Otherwise
## inverse will be computed and cached.
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
