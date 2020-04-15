## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function takes a matrix as an argument, creates a "matrix" object,
## and is able to cache the inverse of that object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## This function checks to see if the inverse of the matrix object has already been 
## solved for.  If there is no stored inverse matrix, this function finds the inverse
## and sets the value of the inverse in the cache.  If the inverse is already cached, 
## it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse
  if(!is.null(inv)) {
    message("getting chached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
