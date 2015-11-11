## The overal objective is to write some functions that cache the
## inverse of a matrix. The makeCacheMatrix makes a matrix and offers
## a set of functions to set/get the matrix and set/get the
## inverse. The cacheSolve function solves the inverse matrix and
## caches the inverse matrix (if it hasn't already been done).

## author: Marc T. Henry de Frahan

## As defined in the assignement:
## "This function creates a special "matrix" object that can cache its inverse."
##
## We define four functions that belong to this function which
## set/get the matrix and set/get the inverse. This allows for caching
## of the matrix inverse which can be an expensive operation (we don't
## want to keep recalculating the inverse if it has already been
## done).
##
makeCacheMatrix <- function(x = matrix()) {
    ## Returns a list of functions to set/get the matrix x and it's inverse
    
    ## initialize the inverse to null
    inv <- NULL

    ## function that stores the matrix and (re)sets the inverse to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## function to return the matrix
    get <- function() x

    ## function to store the inverse matrix in variable inv
    setinverse <- function(inverse) inv <<- inverse

    ## function to return the inverse
    getinverse <- function() inv

    ## return the functions we defined as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## As defined in the assignement:
## "This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache."
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Get the inverse of the matrix x
    inv <- x$getinverse()

    ## Test to see if the inverse has been defined (in which case it's
    ## not null). If it exists (i.e. has already been created), just
    ## return the cached inverse.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Otherwise, the inverse has not been calculated yet, and we
    ## should calcuate it and cache it

    ## get the matrix
    data <- x$get()

    ## get the matrix inverse
    inv <- solve(data, ...)

    ## cache it
    x$setinverse(inv)

    ## return the inverse
    inv    
}
