## Put comments here that give an overall description of what your
## functions do
##
## Assignment 2 - Caching the Inverse of a Matrix

## This function creates a matrix object that provides functions to cache its inverse. 
## The matrix object can be seen (in object-oriented terminology) as a class with 2 attributes 
## (matrix and its inverse) and getter / setter to access these attributes. This matrix object 
## itself does not calculate the inverse. It only holds the information.
## 
## Description of the provided functions:
##
## get() - returns the matrix that is used by this matrix object
## set() - sets a given matrix to be used by this matrix object
## getinverse() - returns the calculated inverse of the matrix.
##                Will return NULL as long as not setinverse() was called
## setinverse() - sets the given calculated inverse of the matrix
##


makeCacheMatrix <- function(x = matrix()) {
  
  ## holds the calculaed inverse of the matrix
  inv <- NULL
  
  ## sets a given matrix and resets a previous calculated inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    x
  }
  
  ## set the given calculated matrix
  setinverse <- function(solve) {
    inv <<- solve
  }
  
  ## get the calculated inverse
  getinverse <- function() {
    inv
  }
  
  ## provides a list of the defined functions (getter / setter)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of a matrix contained in a matrix object
## which was created with the makeCacheMatrix function. It will retrieve information from the
## given matrix object and either calculate the inverse matrix or return the cached inverse matrix.
## If the inverse matrix is calculated it will be set in the matrix object to be cached.
##
## As result it returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
  
  
  ## Retrieve the inverse from the matrix object
  inv <- x$getinverse()
  
  ## If the inverse is not NULL the matrix object contains already a cached inverse.
  ## It that case we just return this cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## To calculate the inverse we first get the matrix from the given matrix object
  data <- x$get()
  
  ## We calculate the inverse by using the solve() function. 
  ## Note: This assumes that a square invertible matrix was created before.
  inv <- solve(data, ...)
  
  ## We set the calculated inverse in the matrix object and return the calculated inverse
  x$setinverse(inv)
  inv
}
