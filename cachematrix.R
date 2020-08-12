## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Given a square invertible matrix, cache the matrix in a list
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function
##First check if the matrix (x) is already in the cached data
cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Return a matrix that is the inverse of 'x'
  dat <- x$get()
  i <- solve(dat,...)
  x$setinverse(i)
  i
}
