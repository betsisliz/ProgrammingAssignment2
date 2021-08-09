## The following functions (makeCacheMatrix(), cacheSolve()) 
## create a special object that stores a matrix and cache's its inverse

## makeCacheMatrix() takes one argument that is a matrix and returns
## a list of functions to pass to cacheSolve()

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

m<-matrix(1:4,2,2)

## cacheSolve() takes the output of the previous function and calculates the 
## inverse of the matrix or returns the cached inverse if it has been calculated

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
