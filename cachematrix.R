## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           #initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {
        x <<- y             #define the set function to assign new 
        inv <<- NULL        #value of matrix in parent environment
                            # if there is a new matrix, reset inv to NULL.
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse #define setinverse()
    getinverse <- function() inv  #define getinverse()
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
