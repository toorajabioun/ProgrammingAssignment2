## MakeacheMatrix creates a special matrix which can store it's inverse.
## CacheSolve takes the above special matrix as an input and returns it's 
## inverse. if the inverse is stored in the special matrix, then this
## funcion does not calculate the inverse, and uses the previousely
## calculated inverse.

## makeCacheMatrix: 
##    inputs: x: x is optional and initialized the matrix
##    functions:  set: sets the matrix
##                get: returns the matrix
##                setinverse: set the inverse
##                getinverse: returns the stored inverse
##    output: a list containing functions set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(a) inv <<- a
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve:
##    inputs: x: a special matrix returened by makeCacheMatrix()
##    output: the inverse of matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (is.null(inv)) {
    M <- x$get()
    inv <- solve(M, ...)
    x$setinv(inv)
  } else {
    message("getting cached data")
  }
  inv
}
