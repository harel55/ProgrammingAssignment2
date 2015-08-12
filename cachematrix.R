## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) I <<- inverse
        getinv <- function() I
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}
