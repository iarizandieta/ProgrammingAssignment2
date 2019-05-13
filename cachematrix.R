## Matrix inversion is usually a costly computation and it 
## needs to be computed repeatedly. The functions in this script
## will provide the user a shortcut to create a matrix and caches its
## inverse. 

## makeCacheMatrix is a shortcut that returns a list containing a matrix
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                im <<- NULL
                x <<- y
        }
        
        get <- function() x
        getinverse <- function() im
        setinverse <- function(inverse) im <<- inverse
        list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)

}


## CacheSolve computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("returning cached inverse")
                im
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
