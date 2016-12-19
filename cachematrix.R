##Functions to create a special "matrix" object with a cached inverse
##and to retrieve the cached inverse if already calculated from inside the object itself

##This function creates the "matrix" object with cached inverse
##Object has internal functions to establish and return itself and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseOfMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseM) inverseOfMatrix <<- inverseM
  getInverse <- function() inverseOfMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##This function extracts cached inverse from a "matrix" object or
##computes and sets the inverse if it was not cached then returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseOfMatrix <- x$getInverse()
  if(!is.null(inverseOfMatrix)) {
    message("getting cached data")
    return(inverseOfMatrix)
  }
  data <- x$get()
  inverseOfMatrix <- solve(data, ...)
  x$setInverse(inverseOfMatrix)
  inverseOfMatrix
}
