# This script implements an "object" that could be called cacheMatrix,
# a matrix that can cache its inverse, avoiding potentially time-consuming
# computations if this inverse is to be used repeatedly.
#
# The first function, makeCacheMatrix, creates a cacheMatrix "object" and
# the second one, cacheSolve, computes the inverse of cacheMatrix "object"
# caching it for future use.
#
# Example:
# m1 <- matrix(sample(-5:5, 16, replace=T), nrow=4)
# cache.m1 <- makeCacheMatrix(m1)
# 
# # First call, cacheSolve will compute the inverse
# inverse.m1 <- cacheSolve(cache.m1)
# inverse.m1
# 
# # Second call on the same matrix, cacheSolve will get the cached inverse
# newinverse.m1 <- cacheSolve(cache.m1)
# newinverse.m1
#
# # You can pass the required parameters to create a matrix
# # directly to makeCacheMatrix
# cache.m2 <- makeCacheMatrix(sample(-5:5, 16, replace=T), nrow=4)
# inverse.m2 <- cacheSolve(cache.m2)
# inverse.m2


# This function creates a cacheMatrix "object" that can cache its inverse.
makeCacheMatrix <- function(x = matrix(), ...) {
  
  # try to coerce x to matrix using passed parameters
  if (!is.matrix(x))
    x <- matrix(x, ...)
  
  # initialize variable that will store the inverse of x
  s <- NULL

  # methods to manipulate internal variables of
  # the cacheMatrix "object"
  # set(), get(), setSolve() and getSolve()
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(solve) s <<- solve
  
  getSolve <- function() s
  
  # return a list with "object" methods
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
}


# This function computes the inverse of matrices returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# it does not recompute the inverse, retrieves it from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getSolve()

  # compute matrix inverse if it is null
  if (is.null(s)) {
    matrix <- x$get()
    s <- solve(matrix, ...)
    x$setSolve(s)
  } else {
    message("getting cached data")
  }
  
  return(s)
  
}