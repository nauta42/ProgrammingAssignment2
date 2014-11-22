## These functions implement a cache for the inverse matrix
## 
## Wrapper for a matrix, implements the cache for its inverse
makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(inv) {
    mInv <<- inv
  }
  getInv <- function() {
    mInv
  }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Calculates the inverse only if the cache is empty
cacheSolve <- function(x, ...) {
  #read the cache
  inv  <- x$getInv()
  if(!is.null(inv)) {
    message("Get cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  #assume that the matrix supplied is always invertible
  inv <- solve(data)
  x$setInv(inv)
  inv
}
