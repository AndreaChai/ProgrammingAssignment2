## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a matrix that can cache its own inverse.
## cacheSolve computes the inverse of the matrix returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function would retrieve the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix is a function that basically stores a list of functions.
## matrix x is stored in the main function. 

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve uses the functions stored in makeCacheMatrix by subsetting the list of functions in makeCacheMatrix. 
## Input of cacheSolve is the object where makeCacheMatrix is stored to compute the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)                          
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i                    
}
