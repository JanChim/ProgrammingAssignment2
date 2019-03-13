## The makeCacheMatrix function creates a special matrix object which can cache
## its inverse for the input

## the function creates an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
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
  if(!is.NULL (inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	

--------------------------------------------------------------------------------
  Testing

# > mat <- makeCacheMatrix(matrix(1:4, 2, 2))
# > mat$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mat$getInverse()
# NULL
# > cacheSolve(mat)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mat)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mat$getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mat$set(matrix(5:8, 2, 2))
# > mat$get()
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8
# > mat$getInverse()
# NULL
# > cacheSolve(mat)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(mat)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > mat$getInverse()
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5 

