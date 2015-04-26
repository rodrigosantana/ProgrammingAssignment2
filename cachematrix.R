## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This first function sets the steps to construct and apply the
## resolution desired to the respective matrix passed by the user...
makeCacheMatrix <- function(x = matrix()) {
    # Set a null variable to receive tha inverse matrix...
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Get the matrix passed by the user in the argument of this
    # function...
    get <- function() x
    # Set the function that will be used on the matrix passed in the
    # argument of this function... Here the exercise are aimed in solve
    # the inverse of any matrix...
    setmatrix <- function(solve) m <<- solve
    # Get the inverse matrix m...
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Write a short comment describing this function

## This function take the matrix passed by user and apply the resolution
## desired. In this case it was conducted a invertion of a matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Set the matrix passed by the argument in the makeCacheMatrix...
    data <- x$get()
    # Invert the matrix...
    m <- solve(data)
    # Set the inverted matrix to m object
    x$setmatrix(m)
    # Return m
    m
}
