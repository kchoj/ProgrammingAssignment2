## The first function, 'makeCacheMatrix' creates a special "matrix" object
## that can cache its inverse. The second function, 'cacheSolve', computes
## the inverse of the special "matrix" returned by the first function. If the
## inverse has already been calculated (and the matrix has not changed), then
## 'cacheSolve' should retrieve the inverse from the cache.



## This function creates a special "matrix" that is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get=get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## This function calculates the inverse of the special "matrix" created with
## the previous function. First, it checks to see if the inverse has already been
## calculated. If so, it gets the inverse matrix from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached value")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
