## The following pair of functions can be used to cache matrices and their
## computed inverses to avoid repeated computations.

## The functions "makeCacheMatrix" creates a special matrix object that caches
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        m <<- inverse
    }
    
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The "cacheSolve" function computes the inverse of the cached matrix
## created by the function "makeCacheMatrix".
## If the inverse of the matrix has already been calculated, then the inverse
## is retrieved from the cache.

cacheSolve <- function(x, ...) {
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
