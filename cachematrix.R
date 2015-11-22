## cachematrix.R
## This script is developed according to the Coursera course requirements of
## R Programming -- Programming Assignment 2: Lexical Scoping
## -------------------------------------------------------------------------
## This script contains two functions makeCacheMatrix and cacheSolve 
## that provides a way to create a matrix as well as calculate and cache 
## its inverse.
## If the matrix does not change, the inverse is calculated just once
## and then is cached. Next time cacheSolve will return the cached
## value of the inverse. If the matrix changes, the inverse will
## again be calculated for the first time. 
## -------------------------------------------------------------------------
## USAGE:
##
## > source("cachematrix.R")
## > mdat <- matrix(c(2, -1,   8, 0.5), nrow = 2, ncol = 2, byrow = TRUE)
## > m <- makeCacheMatrix(mdat)
## > cinv <- cacheSolve(m)
## > cinv
##             [,1]      [,2]
## [1,]  0.05555556 0.1111111
## [2,] -0.88888889 0.2222222
## > m$getInversedMatrix()
##             [,1]      [,2]
## [1,]  0.05555556 0.1111111
## [2,] -0.88888889 0.2222222
## > m2 <- makeCacheMatrix(m$getInversedMatrix())
## > cinv2 <- cacheSolve(m2)
## > cinv2
##      [,1] [,2]
## [1,]    2 -1.0
## [2,]    8  0.5
## > m2$getInversedMatrix()
##      [,1] [,2]
## [1,]    2 -1.0
## [2,]    8  0.5

## IMPLEMENTATION:

## makeCacheMatrix creates a matrix and returns a list of methods, to operate
## this matrix and its cached inverse.
## get retrieves the matrix,
## set saves a new matrix and  assignes null to the cached inverse,
## getInversedMatrix retrieves the inverse of the matrix or null if the 
## inverse is not calculated yet,
## setInversedMatrix saves (caches) the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        get <- function() x
        setInversedMatrix <- function(inv) inversedMatrix <<- inv
        getInversedMatrix <- function() inversedMatrix
        list(set = set, get = get,
             setInversedMatrix = setInversedMatrix,
             getInversedMatrix = getInversedMatrix)
}


## cacheSolve returns the inverse of a square matrix x. 
## If the inverse has already been calculated 
## cacheSolve returnes the cashed value,
## otherwise this function calculate the inverse of x,
## caches and returns the calculated inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInversedMatrix()
        if(!is.null(inv)) {
                # getting cached inverse of matrix x
                # and return the inversion
                return(inv)
        }
		    # otherwise read the matrix, make its inverse,
		    # cache and return the inversion
        data <- x$get()
        inv <- solve(data, ...)
        x$setInversedMatrix(inv)
        inv 
}
