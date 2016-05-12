## Put comments here that give an overall description of what your
## functions do

## This function will return a list that contains four sub-functions.
## One to (re)set the matrix, one to retrieve it,
## one to set the inverse, one to retrieve that too.

makeCacheMatrix <- function(x = matrix()) {
  
  ## The default value of the inverse.
  i <- NULL
  
  set <- function(matr) {
    x <<- matr
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  
  getinverse <- function() i
  
  ## Return the four functions that manage this object.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will check to see if there is a cached inverse.
## If so, it will return that inverse.
## If not, it will calculate the inverse and return it, and also set the cached inverse to the same.
## This function assumes valid input i.e. x$get() isn't "1:4" or "sdad"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  i <- solve(x$get())
  x$setinverse(i)
  i
}
