## The functions makeCacheMatrix and cacheSolve are used in combination to allow
## requests for a matrix inverse after the initial request, to be satisfied from 
## a cache rather than require recomputation.

## makeCacheMatrix  -   creates the functions that manage the matrix cache
## cacheSolve       -   uses the cache management functions of the matrix object 
##                      to satisfy inverse requests after the initial request

## Usage:  Example
##         m <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##         inv <- makeCacheMatrix(m)
##         cacheSolve(inv)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##         cacheSolve(inv)
## getting cached inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##         dg <- matrix(c(13, 14, 15, 16), nrow=2, ncol=2) 
##         inv$set(dg)
##         cacheSolve(inv)
## [,1] [,2]
## [1,]   -8  7.5
## [2,]    7 -6.5
## cacheSolve(inv)
## getting cached inverse
## [,1] [,2]
## [1,]   -8  7.5
## [2,]    7 -6.5
## 

## The function makeCacheMatrix creates a list consisting of the following functions:
## $set         Sets the value of the function's matrix argument x in the cache
## $get         Gets the value of the function's matrix argument x in the cache
## $setinverse  Sets the value of the inverse of x in the cache
## $getinverse  Gets the value of the inverse of x in the cache

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
    
    ## Create the function set to save the matrix to be inverted
    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    
    ## Create the function get to retrieve the matrix to be inverted
    get <- function() x
    
    ## Create the function setinverse to save the matrix inverse  
    setinverse <- function(xinv = matrix()) xinverse <<- xinv
    
    ## Create the function getinvers to retrieve the local inverse
    getinverse <- function() xinverse
    
    ## Return the list of functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve returns a matrix that is the inverse of its special matrix argument.
## On the first call to the function, the inverse is computed using solve and cached locally
## within the function prior to being returned to the caller. On subsequent calls, the
## cached value is returned without invoking solve.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Try to retrieve the cached inverse
    xinverse <- x$getinverse()
    
    ## If the inverse has already been computed, return it
    if (!is.null(xinverse)) {
        message("getting cached inverse")
        return(xinverse)
    }
    
    ## Otherwise: 
    data <- x$get()
    
    ## Compute the inverse
    xinverse <- solve(data)
    
    ## Save the inverse in the cache
    x$setinverse(xinverse)
    
    ## Return the inverse
    xinverse    
}
