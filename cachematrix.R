## Together, these functions create a cache for the inverse of a matrix, so that 
## when computing the inverse of the matrix, if it's already in the cache, it 
## will be retrieved instead of recalculated.

## This function makes a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
          x <<- y
          i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix returned
## by the previous function, such that if the inverse has already been 
## calculated, then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
              message("getting cached data")
              return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
