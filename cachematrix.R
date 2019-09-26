## This program calculates the inverse of a matrix, unless it was already calculated
## in which case, this program will store and be able to find the last matrix
## inversion done using lexical scoping. This program is a variation of the provided
## function given by Roger Peng

## This function creates a vector to get and store the inverse of a matrix
## so as not to have to repeatedly calculate the same inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(invrs) m <<- invrs
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function solves the inverse of a matrix, unless it has been calculated
## in which case it finds the matrix inverse in a cached variable

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
