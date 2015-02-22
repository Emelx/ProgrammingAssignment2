## Massimo Landi 2015-02-22
## The following functions implement a way to compute the inverse of a matrix
## using a cached value to reduce computing time

## Massimo Landi 2015-02-22
## This function creates an environment so that the inverse of a given matrix
## can be cached. In this environment get/set functions are defined that use
## variables in the parent environment.
## The function returns a "special matrix" that is a list of functions which
## allow to set/get the original matrix and to set/get the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Massimo Landi 2015-02-22
## This function calculates the inverse of the "special matrix" returned by
## the above function.
## The inverse is computed only if a cached value is missing and then the
## cached value is used in subsequent calls. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        mtx <- x$get()
        i <- solve(mtx, ...)
        x$setinverse(i)
        i
}
