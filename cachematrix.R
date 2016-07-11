##  Matrix operations are usually be costly for performance of R. The following functions 
##  stored in cache the calculating of the inverse of a matrix, so that if we need this 
##  computer later, and the matrix has not changed we don't need recalculated because 
##  the function is taken from the cache, which previously had been stored.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv.calc <- NULL
    set <- function(y) {
        x <<- y
        inv.calc <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv.calc <<- inverse
    getInverse <- function() inv.calc
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv.calc <- x$getInverse()
    if (!is.null(inv.calc)) {
        message("getting cached data")
        return(inv.calc)
    }
    mat <- x$get()
    inv.calc <- solve(mat, ...)
    x$setInverse(inv.calc)
    inv.calc
}
