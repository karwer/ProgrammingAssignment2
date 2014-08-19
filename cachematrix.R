# Usage:
# m = makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(m) # this will compute matrix inverse
# cacheSolve(m) # this will get inverted matrix from cache
# m$set(matrix(5:8, 2, 2))
# cacheSolve(m) # this will recompute matrix inverse since matrix changed

# Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(newmatrix) {
        x <<- newmatrix
        inverted <<- NULL
    }
    get <- function() x
    set.inverted <- function(newinverted) inverted <<- newinverted
    get.inverted <- function() inverted
    list(set = set, get = get, set.inverted = set.inverted, get.inverted = get.inverted)
}

# Function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverted <- x$get.inverted()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$set.inverted(inverted)
    inverted
}
