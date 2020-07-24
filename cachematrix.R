## Put comments here that give an overall description of what your
## functions do

## The idea of this question is the same as cacheMean.R
## Simply change the function from mean to solve
## Some examples to test the function:
## x = matrix(c(4,0,0,1,0,0,1,0,0,2,2,0,0,0,0,1), 4, 4)
## x = matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)

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

cacheSolve <- function(x, ...) {
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
