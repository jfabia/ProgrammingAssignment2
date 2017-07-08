## This code has provides two functions, makeCacheMatrix and cacheSolve.
##
## The purpose of this code is to calculate the Inverse of a given matrix and
## store the result in the cache. 
##
## If the matrix was not changed and the inverse was already calculated, the
## program will return the result stored in the cache.
##
## To use these two functions in the console, do the following: 
## 1) Assign a matrix to a variable using makeCacheMatrix
## 2) To calculate the Inverse, use cacheSolve([your matrix])
##
## Here is an example of what to type in the console after loading the functions:
## > x <- makeCacheMatrix(matrix(c(1,0,0,2), nrow = 2, ncol = 2, byrow = TRUE))
## > cacheSolve(x)
##
## The code above will result in
##        [,1] [,2]
##  [1,]    1  0.0
##  [2,]    0  0.5
##
## Typing cacheSolve(x) again will result in
## > cacheSolve(x)
##  Getting cached data
##        [,1] [,2]
##  [1,]    1  0.0
##  [2,]    0  0.5
##
##


## This function will store a matrix to the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will calculate the inverse of the matrix stored by the "matrix" in makeCacheMatrix
## If the inverse has already been computed and the matrix has not changed, the function will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
