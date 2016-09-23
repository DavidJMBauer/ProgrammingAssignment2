## Put comments here that give an overall description of what your
## functions do

#The following functions (1) produce a special matix cache it and (2) then calculate its inverse,
# if not allready calculated

## Write a short comment describing this function
# This function creates a matrix that can cache its inverse by:
#- defining the content of the matirx as NULL
#- getting the content of the matrix
#- defining the function inv_matrix
#- getting the value of inv_matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  matrix() <- x$get()
  inv_matrix <- mean(matrix, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}
