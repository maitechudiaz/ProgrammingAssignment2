## The function "makeCacheMatrix" creates an object of type "matrix" and cache its inverse.
# The strategy is the same as in "makeVector" function given as an example (available at original fork: https://github.com/rdpeng/ProgrammingAssignment2)
# (1) set the value of the matrix, (2) get the value of the matrix, (3) set the inverse (using "solve") and (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##  This function computes the inverse of the object of type "matrix" that is returned by the function "makeCacheMatrix".
# The strategy is the same as in "cachemean" function given as an example for the assignment (available at original fork: https://github.com/rdpeng/ProgrammingAssignment2)
# As in "cachemean" function, this function first checks if the inverse has been calculated, and if so it uses "get" to take the inverse from the cache.
# If the inverse has not been calculated, it makes the calculations and put them in the cache using the function "setsolve".

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

