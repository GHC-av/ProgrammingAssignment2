## Given a invertible matrix,
## the following two functions will calculate
## the inverse matrix or retrieve the inverse matrix from the cache.

## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
## (1)get is a function that returns the matrix x stored in the main function.
## (2)set is a function that changes the matrix stored in the main function.
## (3)setinverse is a function that stores the value of the input value (solve) in the  variable m.
## (4)getinverse is a function that returns the value of the variable m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix,
## m calculates the inverse, and x$setinverse(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
