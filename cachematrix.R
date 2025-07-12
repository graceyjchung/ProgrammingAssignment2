## Using 2 functions to cache the inverse of a matrix to reduce computational load 

## creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    
    setinvmatrix <- function(invmatrix) inverse <<- invmatrix
    getinvmatrix <- function() inverse 
    
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## computes the inverse of the special matrix returned by the above function, if the inverse has already been calculated, and the matrix has not changed, the cachesolve retrieves teh inverse from the cache

cacheSolve <- function(x, ...) {
    inverse = x$getinvmatrix
    
    if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinvmatrix(inverse)
    inverse
        ## Return a matrix that is the inverse of 'x'
}
