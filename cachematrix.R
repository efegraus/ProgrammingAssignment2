## Eric. May 27,2016
## These functions save variables and functions to the global environment in 
## order to cahcethem. This would be useful in case you have large computes. 
## Of course you could always break the code up, write to disk and then load 
## it back up again when needed. are saved.
###################################################
## General Function Summary
## Function: makeCacheMatrix stores the function and data into the global 
## environment.
## Function: cacheSolve - accesses the functions and data from makeCacheMatrix
## and generates the inverse of a matrix.
###################################################
## Function makeCacheMatrix
# Store functions and data into the global environment
# Parameters Required: A square matrix or something that can be converted into
# a square matrix using the matrix() function.
## Example
# Set your matrix:
## matd <- matrix(c(2,4, 4,6),nrow =2, ncol=2,byrow=TRUE)
# Cache the functions and the data
## mymatrix <- makeCacheMatrix(matd)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getinverse = getinverse)
}

###################################################
## Function:cacheSolve
## Access the functions and data stored by makeCacheMatrix
## Parameters: the assigned list from makeCacheMatrix
## Example:
## cacheSolve(mymatrix())
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}



