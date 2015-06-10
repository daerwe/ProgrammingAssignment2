## cachematrix contains two functions that together cache and retrieve a matrix's inverse.
##
## makeCacheMatrix converts an invertible matrix into an object that allows its 
## inverse to be cached.
##
## cachesolve returns a matrix's inverse.  If the inverse is cached, then it simply 
## retrieves it.  Otherwise, it computes the inverse and caches it for future use.

## makeCacheMatrix takes a matrix as input and outputs a list of four functions: set, get
## setinverse, and getinverse.  set allows you to set the original matrix (not used here),
## get retrieves the original matrix, setinverse caches the matrix's inverse, and 
## getinverse attempts to retrieve the matrix's inverse


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
             setinverse=setinverse,
             getinverse=getinverse)
}


## cachesolve takes the list of functions from makeCacheMatrix as input and outputs the 
## matrix's inverse.  First, it checks if the inverse is cached.  If so, it simply returns
## it, without recomputing.  Otherwise, it computes the inverse and caches it for future
## use.  

cacheSolve <- function(x, ...) {
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
