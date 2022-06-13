###############################################################################
# Calculates of the inverse of a matrix and stores it in cache
# Works only on invertible matrices
#
# If the inverse of a matrix is found in the cache, fetch it from the cache
# instead of doing the calculation
###############################################################################

# Gets the matrix and its inverse and stores it in a list

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Calculates the inverse of a matrix
# Fetches the inverse from cache if the calculation was done before

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
