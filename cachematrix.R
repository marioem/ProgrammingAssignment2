##     File name: cachematrix.R
##      Revision: 1
##  Date created: 2015-06-21
## Date modified: 2015-06-21
##        Author: marioem
##
## This R script defines two functions implementing caching mechanism for calculation
## of a matrix inverse.
## 
## makeCacheMatrix implements a modified matrix object which enables caching of
## its inverse.
##
## cacheSolve implements providing the inverse of a matrix stored in the object created
## by means of makeCacheMatrix by either retrieving its cached inverse or by calculating
## the inverse and updating the cache at the same function call.

########################################################################################
##
## makeCacheMatrix(x)
##
## This fuction creates an object (or function closure) which provides
## means to store a matrix and its inverse along with the functions for manipulating
## that object.
##
## Input: x - square and inversible matrix object. If no input is provided,
##            an empty matrix object is created.
## Output: list of following functions is returned:
##         get() - funtion with no input arguments, returning the matrix stored
##                 in the object
##         set(y) - function taking a matrix 'y' as an input, returning nothing. It replaces
##                  currently stored matrix in the object with the new one, provided as
##                  an input argument to set(.)
##         getinv() - funtion with no input arguments, returning the inverse 
##                  of the stored matrix, if calculated by calling cacheSolve on this
##                  object and NULL otherwise
##         setinv(inverse) - function taking a matrix 'inverse' as an input, returning nothing.
##                  It replaces currently stored inverse in the object with the new one,
##                  provided as an input argument to setinv(.). It is intended to be used
##                  by cacheSolve function where cacheSolve stores the calculated inverse
##                  matrix to the one stored in the instance of cache matrix object.
##                  Note, that there is no protection against storing arbitrary matrix
##                  by a call to setinv(.) function outside chacheSolve(.)
## Limitations: There is no protection against storing non-matrix, non-square or 
##              non-inversible matrices in this cache matrix object or against caching an
##              inverse which doesn't correspond to the matrix stored in the object.

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

########################################################################################
##
## cacheSolve(x, ...)
##
## This fuction returns a matrix that is the inverse of 'x'. It retrieves the inverse,
## if it exists, otherwise it calculates the inverse and updates the cache. If there
## was a 'cache hit', the informative message is printed out.
##
## Input: x - an object created by means of makeCacheMatrix
##        ... - further arguments passed to or from other methods
## Output: a matrix which is the inverse of the one stored in the object supplied as
##         an input parameter

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    # Update cache only if there is data to cache
    # Note that this is very rudimentary protection and not required by the assignment
    if(!any(is.na(inv)))
        x$setinv(inv)
    inv
}
