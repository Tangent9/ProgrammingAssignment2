## The following functions exercise on the scoping rule of R. More specifically, 
## the inverse of a first-time input matrix is put into cache. When we need it
## again later, it can be looked up in the cache, not being recomputed, saving
## need to redo the costly matrix inverse computation again.

## This function sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function calculates the inverse. It first checks whether the inverse
## is already in the cache. If not, it will perform the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
