#Creates a matrix object which contains the information for the inverse of that
#matrix and the matrix itself and allows the user to set a new matrix
#or cache the inverse of a new matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    #Gives the inverse of matrix x if it is cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    #Computes the inverse of matrix x and caches it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
