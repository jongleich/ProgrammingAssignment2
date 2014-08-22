## Put comments here that give an overall description of what your
## functions do

## This function allow for the caching and  get/set inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## this function allows for getting the cached inverse of the x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
	    if(!is.null(m)) {
	      message("getting cached data")
	      return(m)
	    }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setInverse(m)
  m
}
