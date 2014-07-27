## For performance reasons a function is implemented that caches
## the result of the solve operation to get the inverse matrix 
## if the matrix itself did not change.

## This function creates a structure that stores 
## the maxtrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## define getter/setter for the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## define getter/setter for the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ## return a list of the defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse is for a matrix is already 
## present in the cache. If this is the case the inverse matrix 
## is returned, in any other case the inverse matrix is calculated, 
## stored in the cache and returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()   ## read inverse matrix from cache
  if(!is.null(i)) {     ## if the inverse matrix exists in the cache
    message("getting cached data")
    return(i)           ## return cached inverse matrix of 'x'
  }
  data <- x$get()
  i <- solve(data, ...) ## calculate the inverse matrix
  x$setinverse(i)       ## write inverse matrix to cache
  i                     ## Return calculated inverse matrix of 'x'
}