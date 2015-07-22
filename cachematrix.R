## I have written two functions: 1) makeCacheMatrix() and 2) cacheSolve
## These two functions conspire to calcluate the inverse of a square matrix
## and to store that cache that inverse so it can be recalled instead of
## recalculated when it is needed.
##
## The functions would be used in the following manner:
##   1. Call makeCacheMatrix with a matrix as the argument, storing the 
##      output in a variable 'x'
##   2. Call cacheSolve using the variable 'x' from above as the argument
##   3. Use the function x$get() to retrieve the original matrix
##   4. Use the function x$getinverse() to retrieve the inverse matrix
##   5. Use the function x$set() to replace the values in the starting matrix
##   6. Use the function x$setinverse to manually set values of the inverse matrix
##

## This function makeCacheMatrix() accepts a matrix as an input argument.
## For example, thusly:
##   > mat <- matrix(c(2,1,6,3,2,7,4,5,1),nrow = 3,ncol = 3)
##   > x <- makeCacheMatrix(mat)
##
## It also stores a list of additional functions which can be used to:
##   get: retrieve the matrix passed as the original argument
##   set: replace the existing matrix with a new set of values
##   getinverse: retrieve the values of the calculated inverse
##   setinverse: manually set values that will be retrieved by getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function receives a matrix as its argument.
## It then checks to see if there is already a cached inverse matrix
## and if so, it retrieves the cached matrix.
## If there is no cached inverse, an inverse is calculated and cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}