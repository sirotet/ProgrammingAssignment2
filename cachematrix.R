## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Both following functions are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  set_inv <- function(inv) m <<- inv ## set the cache m equal
  ## to the inverse of the matrix x
  get_inv <- function() m ## return the cached inverse of x
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## Write a short comment describing this function

## The function calculates the inverse of the matrix that created from above function
## First, it checks the matrix, if the inverse has been caclulated. 
## it will get the inverse from the cache and it will not do any computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inv() #m = get_inv
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}