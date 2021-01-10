## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function calculates a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## This function calculates the inverse of the "special" matrix created with makeCacheMatrix() function
## It first checks if it had been already calculated, in order to re-use the value; 
## If it had not been calculated before, it calculates it and caches it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}