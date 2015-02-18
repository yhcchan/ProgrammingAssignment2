## Description 

# The following functions allow the user to create a matrix and obtain its inverse.
# The functions allow the inverse matrix to be cached so as to prevent the program 
# from re-computing the inverse every time the function to retrieve the inverse 
# is called.

# makeCacheMatrix creates a matrix object whose value and inverse can be set,
# stored, and retrieved. The matrix object comprises the matrix
# and a cached copy of its inverse, if provided.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(m) {
            x <<- m
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve takes a matrix object created in makeCacheMatrix as an argumentand 
# computes the inverse of that matrix. If there is already an inverse matrix in 
# the cache, cacheSolve does not compute the inverse but simply returns the cached data.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}
