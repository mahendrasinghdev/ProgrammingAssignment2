## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are created as part of assignment 2. Their function explained below

## Assumption : For this assignment, assume that the matrix supplied is always invertible


## This function creates a special "matrix" object that can cache its inverse
## This creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_x <<- inverse
    getinverse <- function() inv_x
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}


## Returns the inverse of the matrix. It first checks ifthe inverse has already been computed 
## If so, it gets the result and skips the computation
## If not, it computes the inverse.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        message("getting already computed cached data.")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$setinverse(inv_x)
    inv_x
}
