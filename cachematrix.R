# Author: Anthony Checkas
# Version: 1.0
# Date: 2022.12.09

# Summary:
# The functions below combine as a pair in order to compute the inverse
# of a matrix and to cache that solution so that it doesn't need to be
# recalculated every time.

#' Create a special "cacheable matrix"
#'
#' @param x A matrix.
#'
#' @return a
#'
#' @examples
#' x <- matrix(1:4, 2, 2)
#' solveablex <- makeCacheMatrix(x)
#'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Either return the cached solved matrix or solve the "cacheable matrix"
#'
#' @param x A "cacheable matrix" as returned by makeCacheMatrix.
#'
#' @return the inverse of the cacheable matrix
#'
#' @examples
#' solution x <- cacheSolve(solveablex)
#'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
