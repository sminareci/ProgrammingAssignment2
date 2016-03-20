## This function creates a special "matrix" object that can cache its inverse.
## 1. makeCacheMatrix: W3

makeCacheMatrix <- function(x=matrix()) {m <- NULL
         set <- function(y) {x <<- y      
         m <<- NULL}
         get <- function() x
         setInverse <- function(inverse) m <<-inverse
         getInverse <- function() m
         list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve retrieves the inverse from the cache.
## 2. cacheSolve:  W3

cacheSolve <- function(x, ...) {m <- x$getInverse()
        if ( ! is.null(m)) {print("getting cached data")
        return(m)}
        m <- solve(x$get())
        x$setInverse(m)
        m}
        
