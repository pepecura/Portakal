## Code developed for Coursera R Programming Assignment (week #3)
## Purpose: This R code computes the inverse of a matrix and caches the value to return if it is called repeatedly. 
## Date: 21.12.2014 

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix  <- function (x=matrix()) {
    # initialise the value for the inverse.
    inv     <- NULL
    # get the special matrix defined.
    get     <- function () x
    # set the inverse value globally.
    setinv  <- function (imatrix) inv <<- imatrix
    # get the inverse of the special matrix.
    getinv  <- function () inv 
    list (get = get, setinv = setinv, getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve retrieves the inverse from the cache.
cacheSolve <- function (x, ... ) {
    # retrieve the value of the inverse matrix from cache if computed earlier.
    out <- x$getinv()
    # if the inverse value is not null then return the value.
    if (!is.null(out)){
        message("returned cached value")
        return(out)
    }
    # if the inverse value is null then compute the inverse.
    message("returned new computation")
    # retrieve the special matrix.
    data <- x$get()
    # solve the inverse of the matrix.
    out <- solve(data, ...)
    # set the new value for the inverse matrix computation in global environment. 
    x$setinv(out)
    out
}

## TEST run script:
# rm(list = ls())
# source("./cachematrix.R")
# mymat <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# cacheSolve(mymat)
# cacheSolve(mymat)

## END OF FILE ##
