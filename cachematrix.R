##Coursera assignment 2
##Caching the inverse of a matirx

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 b <- NULL
 set <- function(y) {
     x <<- y
     b <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) b <<- inverse
 getinverse <- function() b
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
 
}

## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    b <- x$getinverse()
    if(!is.null(b)) {
        message("getting cached data")
        return(b)
    }
    data <- x$get()
    b <-solve(data,...)
    x$setinverse(b)
    b
}
