## The below two functions are to cache the Inverse of a matrix and if the
## inverse is already cached for an input, then pull that from cache instead of
## recalculating

## makeCacheMatrix is to cache the Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y)
        {
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


## cacheSolve function computes the Inverse of a Matrix using solve() function
## If the inverse is already calculated, it will be pulled from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
