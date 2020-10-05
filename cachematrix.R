## In order to avoid computing time for demanding process like inverting a matrix
## this function caches its parameters by taking advantage of **Scoping Rules** concept.

## Function that cache the function parameter, in this case the parameter will be a matrix

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


## solve generic function is used in order to inverse a matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                    message("getting cached matrix")
                    return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
