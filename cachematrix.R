## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix unchanged), then the cacheSolve should retrieve the
## inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve(x)
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of a square matrix provided.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
