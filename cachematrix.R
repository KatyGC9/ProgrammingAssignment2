## These functions will first create a matrix, then cache the inverse of the matrix or retrieve the inverse if it has been cached

## This function creates a matrix to be cached

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <-function(x)
    setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function()inverse
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
  
}


## This function will solve for the inverse of the matrix, or retrieve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse
  if (!is.null(inverse)){
    message("get cached inverse")
    return(inverse)
  }
  solution <- x$get()
  inverse <- solve(solution, ...)
  x$setinverse(inverse)
  solve(x)
}
