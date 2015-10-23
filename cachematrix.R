##  "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse. Object has several functions that allows to set up
## "matrix" values, read "matrix" values, read cached inverse of "matrix" or set by user "inverse matrix". Inverse "matrix" is calculated by "CacheSolve" funtion below.
##  "CacheSolve" function computes the inverse of the "matrix" returned by "makeCacheMatrix" funtion above. If the inverse has already been calculated 
## (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.
## If the inverse was not calulated or user changed "Matrix" after previous calulation, funtion will calculate "inverse" matrix.


## "makeCacheMatrix" function creates list of functions: "set" - to set values of matrix "x", "get" - to set values of matrix "x"
## , "setinverse" - to set "inverse" of matrix "x" by user (not to calculate by "cacheSolve"), "getinverse" - to get inverse of matrix "x" 
## which was calculated by "cacheSolve" or entered by used

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(z) inverse<<- z
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## "cacheSolve" function returns inverse of matrix "x". It checks value of function "getinverse" of "x" and return this cached value it if it is not null. 
## In case of "getinverse" of "x" equals null function calculatetes inverse of "x" matrix.

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
