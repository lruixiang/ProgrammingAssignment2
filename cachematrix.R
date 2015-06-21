## Put comments here that give an overall description of what your
## functions do

## The following 2 functions, 1:makeCacheMatrix and 2:cacheSolve, are designed to
## reduce the computation time required to solve for the inverse of a matrix.
## The computation for the inverse of a matrix will be dealt with by 2:cacheSolve,
## and 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(x,...) ## argument "b" is missing, hence taken to be identity matrix
        x$setinverse(i)
        i
}
