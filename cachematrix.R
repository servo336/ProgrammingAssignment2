## R Assignment 2: Caching the Inverse of a Matrix (by: Servillano Poserio)
## Function Description: This R script is compose of a pair of functions 
## that cache the inverse of a matrix.

## The makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## Test data: x=rbind(c(3,1),c(4,2))

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setMatrixInverse<- function(m_inverse) matrix_inverse <<- m_inverse
  getMatrixInverse <- function() matrix_inverse
  list(set=set, get=get, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
  
}

## The cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  matrix_inverse <- x$getMatrixInverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data.")
    ## Return a matrix that is the inverse of 'x'
    return(matrix_inverse) 
  }
  m_data <- x$get()
  matrix_inverse<- solve(m_data)
  x$setMatrixInverse(matrix_inverse)
  matrix_inverse
  
}
