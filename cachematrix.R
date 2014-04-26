## azpmerrill assignment   ---Put comments here that give an overall description of what your
## Computing the inverse of a square matrix can be done with the solve function in R. For example, 
## if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM  <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) invM <<- solve
        getSolve <- function() invM 
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getSolve()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- x$get()
        invX  <- solve(data, ...)
        x$setSolve(invX)
        invX 
}
