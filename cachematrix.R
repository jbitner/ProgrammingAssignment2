## Put comments here that give an overall description of what your
## functions do

## set = sets the value of the matrix stored in the main function
## get = returns the value of the matrix stored in the main function
## setinverse = sets the value of the inverse of the matrix
## getinverse = returns the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function returns the inverse of the matrix
## First it checks to see if the value already is stored if it is not it calculates it and stores it
cachesolve <- function(x, ...) {
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
