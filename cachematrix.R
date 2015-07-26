## This file contains two functions for handling and 
## inverting matrices in an efficient way
##
## The first function will construct a special matrix
## The second function will use the first "object" and
## store a cached version of its inverse


## makeCacheMatrix
##
## this function takes a matrix and stores it internally.
## it has getters and setters which can be invoked externally
## moreover, it can store (and save/cache) its own inverse
## so that this does not need to be calculated over and over
##
## parameters: x matrix()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve
##
## this function "solves" a matrix by calculating its
## inverse. the matrix must be constructed by the function
## makeCacheMatrix described above.
## it first checks whether there is already a stored
## version of the inverse, and if not it will be calculated
## and returned
## in any case the inverse will be returned (if invertible)
##
## preconditions: matrix must be invertible
##
## paramters x matrix from makeCacheMatrix described above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
