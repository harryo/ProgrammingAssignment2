## These functions calculate the inverse of a matrix and cache the result.
## On subsequent calls, the cached will be used to avoid repetitive calculations

## Using the given matrix, create a list of functions to do the caching

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Calculate the inverted matrix, or return the cached value if calculated already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


## Sample use:

## > # Create a sample matrix
## > m <- matrix( 1:4, 2, 2)
## > m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > # Make a cacheMatrix
## > cm <- makeCacheMatrix(m)
## > # Apply the function to solve it
## > cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > # Apply again to check the use of cache
## > cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
