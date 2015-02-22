## Since matrix inversion is a costly computation,  makeCacheMatrix is a special type of Matrix
## with its inverse cached.  The cached inverse is saved (cached) when it's calculated for the first
## time and is invalidated when the matrix data changes (via the set function)
##
makeCacheMatrix <- function(x = matrix()) {
    x.inverse.cached <- NULL
    
    ## set function is used to updated matrix data and invalidate inverse cached
    set <- function(y) {
        x <<- y
        x.inverse.cached <<- NULL
    }
    
    ## get function is used to return matrix data
    get <- function() x
    
    ## setInverse is used to update its parent context inverse cached
    setInverse <- function(inverse) x.inverse.cached <<- inverse
    
    # getInverse is used to return the inverse cached
    getInverse <- function() x.inverse.cached
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of matrix x from either a cached value or copmute the inverse 
## dynamically and save it to the inverse cached
##
cacheSolve <- function(x, ...) {

    ## First check if the inverse cached exist
    x.inverse <- x$getInverse()
    if(!is.null(x.inverse)) {
        message("getting cached inverse")
        ## Return a matrix that is the inverse of 'x'
        return(x.inverse)
    }
    
    ## inverse cache does not exist so compute it then save it to the inverse cache
    data <- x$get()
    x.inverse <- solve(data, ...)
    x$setInverse(x.inverse)
    
    ## Return a matrix that is the inverse of 'x'
    x.inverse
}
