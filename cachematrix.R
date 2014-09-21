## Please find below the overall description of the functions
## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## get it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) 
        {
            x <<- y
            i <<- NULL
        }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, if its 
## not available, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}
