## Try editingPut comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y   ## value of matrix in parent environment
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve   ## assigns value of inv in parent environment
  getsolve <- function() m
  list(set = set, get = get,    ## you need this in order to refer to the functions with the $ operator
       setsolve= setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated ,
## then cacheSolve will retrieve the inverse directly from the cache


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
