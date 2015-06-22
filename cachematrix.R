#  Assignment of Caching the Inverse of a Matrix
 ## These pair of functions can cache the inverse of a matrix calculation, 
 ##  rather than compute it repeatedly
 ##  (maybe beneficial because matrix inversion is usually a costly computation)

# Creates a special "matrix" object 
makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(y) {
        x <<- y
        x.inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x.inverse <<- inv
    getinv <- function() x.inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# Return a matrix that is the inverse of 'x' (and compared in the cache of makeCacheMatrix)
cacheSolve <- function(x, ...) {
    x.inverse <- x$getinv()
    if(!is.null(x.inverse)) {
        message("getting cached data")
        return(x.inverse)
    }
    data <- x$get()
    x.inverse <- solve(data, ...)
    x$setinv(x.inverse)
    x.inverse
}

# test <- matrix(c(1, -1, -1, -1, 2, 3, 1, 1, 4), 3, byrow=TRUE);    test
# result <- makeCacheMatrix(matrix(c(1, -1, -1, -1, 2, 3, 1, 1, 4), 3, byrow=TRUE))
# cacheSolve(result)
# cacheSolve(result)
