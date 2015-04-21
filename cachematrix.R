##                   Assignment 2
## This code is developed as part of R-Programming course offered by John Hopkins Univ.
## 

## These R functions are able to cache potentially time-consuming computations. 
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## The pair of functions in this file cache the inverse of a matrix.


# makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
          # Initialise Inverse Matric (im) to NULL
          im <- NULL
          ## save if the matrix changes and initialise im to NULL
          set <- function(y) {
            x <<- y
            im <<- NULL
          }
          ## retrieve the matrix
          get <- function() x
          ## cache the inverse of matrix
          setinvmatrix <- function(invmatrix) im <<- invmatrix
          
          ## retrieve the cashed matrix
          getinvmatrix <- function() im
          
          ## return member functions
          list(set = set, 
               get = get, 
               setinvmatrix=setinvmatrix, 
               getinvmatrix=getinvmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Try to retrive cashed inverse matrix
      im <- x$getinvmatrix()
      if(!is.null(im)) {
          message("getting cached inverse matrix")
          return(im)
      }
      ## if not found in cache compute inverse matrix, cache it and then return it.
      mat <- x$get()
      im <- solve(mat, ...)
      x$setinvmatrix(im)
      im
}
