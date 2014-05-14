## These functions make use of global variables to cache and retrieve the inverse of a special matrix object.

## makeCacheMatrix takes a matrix as an argument and returns a list of four functions that allow you to set and retrieve the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list (set=set, get=get,
              setsolve=setsolve,
              getsolve=getsolve)
}



## cacheSolve is a client function that takes a matrix created by the "makeCacheMatrix" function. It then 1) queries if there is cached data and returns the cache if there is, or 2) computes the matrix inverse if there is no cached data,  saves the result back to x's cache, and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
                message ("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
