## This script is developed according to the Coursera course requirements of
## R Programming -- Programming Assignment 2: Lexical Scoping
## ADD BFIEF DESCRIPTION of WHAT thees
## functions do
## functions do
## functions do

## Write a short comment describing this function

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
        ## Return a matrix that is the inverse of 'x'
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
