## Copyright 2014 Arun K Viswanathan
## All rights reserved

## R Programming course - programming assignment 2
## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Gets the matrix
    get <- function() {
        x
    }
    
    ## Sets the matrix and also resets the cached inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL 
    }
    
    ## Gets the inverse of the matrix
    getinverse <- function() {
        inverse
    }
    
    ## Caches the inverse of the matrix
    setinverse <- function(inverse) {
        inverse <<- inverse
    }
    
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (is.null(inverse)) {
        inverse <- solve(x$get(), ...) 
        x$setinverse(inverse)
    } 
    inverse
}
