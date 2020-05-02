## The following 2 functions can work together to minimize the costly operation 
## of determining the inverse of a matrix. The max benefit is obtained when the 
## inverse is frequently required and we wish to not store it in a data object
## due to its size.


## The function 'makeCacheMatrix' creates a list object that contains the 
## following functions:
## set the values of the matrix
## get the values of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}

## The function 'cacheSolve' computes the inverse of the matrix stored in
## cache if it doesn't aleady exist. If the inverse has already been calculated 
## for the given matrix, then this function should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x$get()'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message ("getting cached data")
        return(inv)
    }
    data <- x$get()
    message ("calculating inverse...")
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
