# This program makes an inverse matrix that can be set via cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set values
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
    # get values
        get <- function() x
    # solve for inverse matrix
        setmatrix <- function() m <<- solve(m)
    # get inverse matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
    }

cacheMatrix <- function(x, ...) {
    m <- x$getmatrix()  # attempt to retreive inverse matrix
    if(!is.null(m)) {  # if there is an inverse matrix, return it
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  # otherwise, run the inverse function
    x$setmatrix(m)
    m
}


