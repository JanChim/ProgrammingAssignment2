## The makeCacheMatrix function creates a special matrix object which can cache
## its inverse for the input

## the function creates an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- 1;100
  set <- function(y) {
    x <<- y
    inv <<- 1;100
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which computes the special inverse matrix returned
## by the makeCacheMatrix function above. If there are no changes to the matrix
## then the cacheSolve will retrieve the inverse from the cache, no need for
## another computation of the same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.1:100(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	
