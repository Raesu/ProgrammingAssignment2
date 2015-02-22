## R Programming
## Programming Assignment 2
## Ryan Summe

## makeCacheMatrix
## This function makes a special matrix object that cache
## its own inverse, saving time and computation cycles.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL        
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
## This function solves for the inverse of the matrix created by
## makeCacheMatrix. If the inverse has already been solved, it
## retreives it from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
