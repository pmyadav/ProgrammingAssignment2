##                   Assignment 2
## This code is developed as part of R-Programming course offered by John Hopkins Univ.
## 

## These R functions are able to cache potentially time-consuming computations. 
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## The pair of functions in this file cache the inverse of a matrix.


# makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
          im <- NULL
          set <- function(y) {
            x <<- y
            im <<- NULL
          }
          get <- function() x
          setinvmatrix <- function(invmatrix) im <<- invmatrix
          getinvmatrix <- function() im
          list(set = set, 
               get = get, 
               setinvmatrix=setinvmatrix, 
               getinvmatrix=getinvmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinvmatrix()
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  mat <- x$get()
  im <- solve(mat, ...)
  x$setinvmatrix(im)
  im
}
