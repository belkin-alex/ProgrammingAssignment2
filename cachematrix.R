# This script uses the inv() function in the 'matlib' package to calculate, cache, and retreive the inverse of a matrix.

install.packages('matlib')
library(matlib)

## This function, based on the vector mean example, creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                      x <<- y
                      i <<- NULL
              }
              get <-  function() x
              setInverse <- function(inverse) i <<- inverse
              getInverse <- function() i
              list(set = set, get = get,
                   setInverse = setInverse,
                   getInverse = getInverse)
}



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          i <- x$getInverse()
          if(!is.null(i)) {
            message("getting cached data")
            return(i)
          }
          data <- x$get()
          i <- inv(data, ...)
          x$setInverse(i)
          i
}

