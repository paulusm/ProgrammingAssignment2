## Closure created with a specific matrix argument 
## Returns accessor functions for the
## matrix argument and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # variable to hold the inverse
  mtrx <- NULL
  
  # setter - initialise the argument matrix and reset the inverse
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  # getter - return the argument matrix
  get <- function() x
  
  #setter - set the inverse
  setinverse <- function(inv) mtrx <<- inv
  
  #getter - get the inverse
  getinverse <- function() mtrx
  
  #return the getter and setter functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function operates on the closure 
cacheSolve <- function(x, ...) {
  # see if inverse is in the cache
  mtrx <- x$getinverse()
  
  # If the inverse isavailable we return it and exit.
  if(!is.null(mtrx)) {
    message("getting cached data")
    return(mtrx)
  }
  
  # Otherwise we get the data, calculate the inverse and store that back in the
  # closure. Return the newly calculated inverse
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setinverse(mtrx)
  mtrx
}
