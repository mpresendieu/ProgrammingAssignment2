## These pair of functions are meant to cache the inverse of a matrix.
## In computing the inverse of a matrix, it was done with with a "solve" function in R, which helps inverse the matrix.

## This function creates a special "matrix" object that can cache (a collections of items fo the same type) its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function (solveMatrix) inv <<- solveMatrix
        getInverse <- function () inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" which is returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
