## Functions in this file allow to store a matrix in a cache, calculate
## an inverse matrix and store it in the cache too.

## Function represents cache which stores a matrix and its inverse.
## Arguments:
##   inputMatrix - invertible matrix
## Return:
##   Function returns list of available functions:
##   setMatrix(inputMatrix) - sets a matrix in cache
##   getMatrix() - returns a matrix from cache
##   setInverseMatrix(inverseMatrix) - sets an inverse matrix
##   getInverseMatrix() - returns an inverse matrix
makeCacheMatrix <- function(inputMatrix = matrix()) {
    cachedInverseMatrix <- NULL
    
    setMatrix <- function(inputMatrix) {
        inputMatrix <<- inputMatrix
        cachedInverseMatrix <<- NULL
    }
    
    getMatrix <- function() {
        inputMatrix
    }
    
    setInverseMatrix <- function(inverseMatrix) {
        cachedInverseMatrix <<- inverseMatrix
    }
    
    getInverseMatrix <- function() {
        cachedInverseMatrix
    }
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Function returns an inverse matrix from a cache.
## If cache is empty, calls solve function, sets result in the cache 
## and returns the inverse matrix.
## Arguments:
##  cache - cache object returned from makeCacheMatrix function
## Return:
##  inverse matrix
cacheSolve <- function(cache) {
    inverseMatrix <- cache$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    inputMatrix <- cache$getMatrix()
    inverseMatrix <- solve(inputMatrix)
    cache$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
