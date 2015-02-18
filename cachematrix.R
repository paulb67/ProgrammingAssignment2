
# Caching function
# Input  : a square matrix, eg matrix(c(1,4,2,7),nrow=2))
# Output : a list containing four functions which set and get the matrix and
#          its inverse within an environment which persists
# On the first call to this function, the original matrix is stored using
# the set() function, eg m1 <- makeCacheMatrix(m)
# When get() is called, this original matrix is returned.
# When setinverse() is called, the inverse is stored in "cache", this closure
# When getinverse() is called (and setinverse has already stored the inverse)
# then this inverse is returned without recalculation
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
                x <<- y
                m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Solve function using cache if it exists, or populating cache if it does not
# Second and subsequent invocations on the same matrix object will respond from
# the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("retrieving from cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
