## My function intends to cache the inverse of a Matrix followed by a function that computes the 
## inverse of the special "matrix" returned by my first function.


## This function stores the inverse of a matrix in a cache

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


## This function access the cached matrix and return the inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached matrix data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

