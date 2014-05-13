## A set of functions for compute, cache and retrieve the 
## inverse of a matrix.
## 
## makeCacheMatrix - compute and cache the inverse.
##                   Assumed the matrix is invertible.
## cacheSolve - calls makeCacheMatrix to either retrieve the 
##              cached value of the inverse, or if this is the
##              first time it is called, it will compute the inverse
##              cache the value, and return the inverse.
##              It is assumed that any matrix passed to the function
##              is invertible.

## factory function for compute and cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL

   set <- function(y) {
      x <<- y
      i <<- NULL
   }

   get <- function() x

   setInverse <- function(inv)  i <<- inv

   getInverse <- function() i

   list(set = set, get = get, setInverse = setInverse,
        getInverse = getInverse)
}


## Function to get the matrix inverse from makeCacheMatrix

cacheSolve <- function(x, ...) {
   i <- x$getInverse()
   if (!is.null(i)) {
      message("getting cached value")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setInverse(i)
   i
}
