## First function makeCacheMatrix creates list of functions that are used to set
## matrix, get matrix, set inverse of matrix and get inverse of matrix.
## Second function checks to see if the inverse has already been calculated. If 
## so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinv function

## Creating list of functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        I <<- NULL
    }
    get <- function() x
    setinv <- function(I) inv <<- I
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)

}


## Calculate mean if not present in cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv                 ## Return a matrix that is the inverse of 'x'
}
