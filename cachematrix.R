## The following functions help to manage a matrix, with the aim to cache the results of solve() on the matrix
## for efficiency.

## Wrapper for a matrix. 
## get: returns the value of the matrix.
## set: replaces the value of the matrix and clear any cached inverse.
## getinverse: returns the cached value of the inverse. NULL if not set yet.
## setinverse: replaces the value of the cached inverse.

## Thanks for viewing!

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
     
    get <- function() x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
     
    getinverse <- function() inverse
    setinverse <- function(i) {
        inverse <<- i
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of given cacheMatrix.
## The cached answer is given for subsequent calls.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()

    if (!is.null(i)) {
        return(i)
    }

    mtx <- x$get()
    i <- solve(mtx, ...)

    x$setinverse(i)
    i
}
