## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversion <- function(solve) m <<- solve
    getinversion <- function() m
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)    

}


## This function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinversion()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion(m)
    m   
    
    ## Return m as a matrix that is the inverse of 'x'
}
