## Assumptions made:
## 1) Matrix supplied to function is always invertible

## The following 2 functions, 1:makeCacheMatrix and 2:cacheSolve, are designed to
## reduce the computation time required to solve for the inverse of a matrix if 
## the inverse has been solved before. 

## makeCacheMatrix is a function that creates a list containing 4 functions
## 1) set = set the value of the matrix
## 2) get = get the value of the matrix
## 3) setinverse = set the value of the inverse of the matrix
## 4) getinverse = get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { ## creating object that stores as matrix
    i <- NULL ## assign NULL to i in global environment
    set <- function(y) { 
        x <<- y ## assign y to x in local environment
        i <<- NULL ## assign NULL to i in local environment
    }
    get <- function() x ## x is evaluated expression of function get
    setinverse <- function(inverse) i <<- inverse ## assign argument inverse to i in local environment
    getinverse <- function() i ## i is evaluated expression of function getinverse
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse) ## returns list of 4 matrices
}

## cacheSolve is a function that calculates the inverse of the matrix created by
## the makeCacheMatrix function. It first checks to see if the inverse has already 
## been computed before. If yes, then it will retrieve the inverse matrix solution
## from the cache and skip the computation. Else, it computes the inverse matrix
## solution and sets the value of the matrix via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() ## assign value of computed inverse of x, to variable i
        if(!is.null(i)){ ## if value does not exist,  
          message("getting cached data") ## return custom message and,
          return(i) ## return NULL
        }
        data <- x$get() ## assign value of matrix x to variable data
        i <- solve(data,...)## compute the inverse of matrix,
                            ## argument "b" is missing, hence taken to be identity matrix
        x$setinverse(i) ## set matrix value i to be setinverse vector of x
        i ## return matrix i
}
