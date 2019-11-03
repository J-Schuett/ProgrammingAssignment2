## With the two following functions we provide the functionality to
## combine an invertible matrix with a cache of its inverse matrix.
## The first function makeCacheMatrix enables us to set and get the 
## matrix and its inverse matrix. The second function cacheSolve
## can then calculate and set the inverse matrix or, if it was 
## already cached, retrieve it.


## The function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set a matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

## makeCacheMatrix takes a matrix as argument.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a "matrix" as given by
## makeCacheMatrix. If the inverse matrix was cached before,
## it is simply returned.
## Otherwise the inverse is calculated and cached in the "matrix".
## The "matrix" is assumed to be invertible.

## cacheSolve takes the result of a call of makeCacheMatrix as argument.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    A_inv <- x$getinverse()
    if(!is.null(A_inv)) {
        message("getting cached inverse matrix")
        return(A_inv)
    }
    A <- x$get()
    A_inv <- solve(A)
    x$setinverse(A_inv)
    A_inv
}
