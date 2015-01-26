
## The following functions cache the inverse of a matrix, so that a
## costly computation of a desired inverse is avoided when unnecessary.
## 
## The inverse is computed only if it has not already been computed
## and available in the cache, or if the matrix itself has changed.


## This function creates a "matrix" object, which is a list of four
## functions to (a) set the value of a matrix, (b) get or return the  
## value of this matrix, (c) set the inverse of this matrix, and 
## (d) get/return the value of this inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv_x <<- inverse
  
  getInverse <- function() inv_x
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function returns the inverse of a "matrix" created using
## the "makeCacheMatrix()" function. If the inverse is already
## available in the cache, it is retrieved without a computation.
##
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if( !is.null(inv) ) {
    message("getting cached data")
    
    return( inv )
  }
  
  A <- x$get()
  inv_A <- solve(A)
  x$setInverse(inv_A)
  
  inv_A
}
