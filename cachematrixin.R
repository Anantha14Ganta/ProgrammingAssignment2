makeMatrix <- function(x = matrix()) {
    ## a Square invertable matrix
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) iv <<- inverse
    getinverse <- function() iv
    ## this list is used as the input to cacheinverse()
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheinverse <- function(x, ...) {
    ## return: inverse of the original matrix input to makeMatrix()
    iv <- x$getinverse()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    iv   ## Return a matrix that is inverse of x
}