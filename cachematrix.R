# makeCacheMatrix function creates a special "matrix", which is
# really a list of functions to
#
#  set the matrix
#  get the matrix
#  set the solve of the matrix
#  get the solve of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# The following function solves the special "matrix"
# created with the above function. However, it first checks to see if the
# result has already been calculated. If so, it gets the result from the
# cache and skips the computation. Otherwise, it solves the matrix and
# sets the result in the cache via the setsolve function.
#
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if (! is.null(s)) {
        message("getting cached result")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
