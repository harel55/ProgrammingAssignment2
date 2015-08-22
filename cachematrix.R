## These functions allow users to create a matrix wrapper which caches its
## inverse after the first calculation, saving time on future calculations.

## Make variables for the matrix and its inverse and store them in the
## calling environment of the function, then return a list of functions
## used to access and modify them. By default, the matrix is empty, though it
## can be specified when calling the function. Setting the matrix again will
## clear the cached inverse, forcing it to be recalculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Take matrix wrapper as input, check if inverse has already been calculated,
## and - if not - calculate it. Returns matrix inverse. Extra arguments are
## given to solve() function. Assumes matrix is invertible.
## 
## Side-effects: prints message if cached inverse is found.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
