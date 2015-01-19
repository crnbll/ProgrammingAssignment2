################################################################################
## Functions that allow to cache the computation of the inverse of a matrix.
## Example of usage:
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8); h8
## cacheableMatrix <- makeCacheMatrix(h8)
## cacheSolve(cacheableMatrix)
################################################################################

## Function that creates a list containing functions to
## get and set the values of a matrix and of its inverse.
## Takes an invertible matrix as an argument.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialization
    i <- NULL
    
    ## Get the value of the matrix
    getMatrix <- function() {
        x
    }
    
    ## Set the value of the matrix
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Get the value of the inverse of the matrix
    getInverse <- function() {
        i
    }
    
    ## Set the value of the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Return a list containing the functions to get and set
    ## the values of the matrix and of its inverse
    list(getMatrix = getMatrix,
         setMatrix = setMatrix,
         getInverse = getInverse,
         setInverse = setInverse)
}

## Function that computes and caches the value of the inverse of a matrix,
## or retrieves this value from the cache if it has been computed already.
## Takes a list created by the above makeCacheMatrix function as an argument.
cacheSolve <- function(x, ...) {

    ## Attempt to get the cached value of the inverse of the matrix
    i <- x$getInverse()
    
    ## If the value of the inverse of the matrix has been cached
    if(!is.null(i)) {
        
        ## Generate a diagnostic message
        message("Now returning the cached value")
        
        ## Return the cached value of the inverse of the matrix
        return(i)
    }
    
    ## If not, get the value of the matrix
    data <- x$getMatrix()
    
    ## Generate a diagnostic message
    message("Now computing, caching and returning the value")
    
    ## Compute the value of the inverse of the matrix
    i <- solve(data, ...)
    
    ## Set the cached value of the inverse of the matrix
    x$setInverse(i)
    
    ## Return the newly cached value of the inverse of the matrix
    i
}