## Put comments here that give an overall description of what your
## functions do
## These are the functions for programming assignment 2 of R Programming
## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

## Write a short comment describing this function
## this function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_x <<- solve
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## this function computes the inverse of the matrix returned by make CacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data,...)
  x$setinv(inv_x)
  inv_x
}
