## There are two functions in this R file: makeCacheMatrix function and cacheSolve function.

## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        
  m <- x$getsolve()
  if(!is.null(m)) {                 ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)       
  m
  ## Return a matrix that is the inverse of 'x'
}
