
## results in an object, makeCacheMatrix, that contains four functions: set(), get(), 
## setmatrix(), and getmatrix(). It also includes the two data objects, x and m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <-function(y){
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list (set = set, get = get,
              setmatrix = setmatrix,
              getmatrix = getmatrix)
}


## Computes the matrix inversion  using solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if (!is.null(m)) {
                message("cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
