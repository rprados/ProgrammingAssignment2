## Functions to cache matrix inverses

## Object with an attribute and four methods
## This is a wrapper class for matrix object in R
## - inverse is the attribute containing the matrix inverse
## - get and set are for input matrix
## - getinverse and setinverse are for output matrix inverse
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


## Cacheable solve function for matrices
## A CacheMatrix object is required
## The function check if the inverse is already computed
## If so the inverse is returned from cache stored in CacheMatrix
## If not the inverse is calculated by solve function 
## and stored in CacheMatrix
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