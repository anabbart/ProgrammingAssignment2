# MATRIX INVERSION ASSIGNMENT: 2 FUNCTIONS TO CACHE INVERSE OF A MATRIX
# makeCacheMatrix creates list with set/get values of matrix an then its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve returns the inverse of a matrix first checking if computed, uses if true, if not computes the
# inverse and sets value in cache
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
