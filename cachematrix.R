## This file contains a pair of functions that
## cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## If set is called with argument y, then assign y to be the new value 
  ## of matrix x and set inv to NULL.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get has no arguments and simply returns the matrix x
  get <- function() {x}
  
  ## setinverse takes one argument and sets inv to have that value.
  setinverse <- function(inverse) {inv <<- inverse}
  
  ## getinverse has no arguments and returns inv
  getinverse <- function() {inv}
  
  ## Return a list of all set and get functions
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by 'makeCacheMatrix'. 
## If the inverse has already been calculated (and 
## the matrix has not changed), then 'cacheSolve' retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'. NOTE: this
  ## function assumes an invertible matrix.
  
  ## Assign the inverse to variable "inv" and check to see whether
  ## it is empty. If not, return the cached inverse matrix.
  inv <- x$getinverse()
  
  ## If inv is not empty, then return the value of inv
    if(!is.null(inv)) {
    message("getting cached data")
    print("getting cached data")
    return(inv)
  }
  
  ## If inv is empty, retrieve the matrix and calculate the  
  ## inverse of the matrix using solve(). Set this as the  
  ## inverse value in makeCacheMatrix. Then return inv.
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
