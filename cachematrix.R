## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  f <- function(y){
    x <<- y
    inv <<- NULL
  }
  g <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(f = f, g = g, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("cached data")     ## line 23: if the inverse is already solved and retrieved from the cache the "cached data" 
    return(inv)                ## message will be displayed and the inverse would be returned.     
  }
  mtx <- x$g()                 
  inv <- solve(mtx, ...)       ## line 27: otherwise the inverse would be solved and set the value of the invrse in the cache.
  x$setInverse(inv)
  inv
  }     
