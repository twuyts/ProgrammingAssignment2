## Matrix inversion is usually a costly computation and 
## their may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.
## This pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object of the given matrix
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## s contains the cached inverse of the given matrix
    s <- NULL
    
    ## the set function can be used to set a new matrix
    ## doing this will 'invalidate' the cache (set it to NULL)
    ## Note: for more info on the <<- construct,
    ##       have a look at http://cran.r-project.org/doc/manuals/r-release/R-intro.html#Scope
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## the get function simply returns the original matrix
    get <- function() x
    
    ## setsolve stores the result in the s variable
    setsolve <- function(solve) s <<- solve
    
    ## getsolve returns the cached inverse
    getsolve <- function() s
    
    ## return a list with all these functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## get the cached value from the object
    s <- x$getsolve()
    
    ## we have a value, return it
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    
    message("nothing in cache, calculating data")    
    ## get the original matrix
    data <- x$get()
    ## calculate the inverse
    s <- solve(data, ...)
    ## store the inverse in the cache
    x$setsolve(s)
    s
}
