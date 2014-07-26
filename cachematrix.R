## This file contains three functions that work with matrices
## Overall, a special matrix is created that caches the inverse of the matrix
## The inverse is then retrieved from the cache whenever it is stored
## In case of change in value of matrix, the inverse is recomputed and then cached

## This function creates a special matrix that caches its inverse 
## The function also provides methods to set the base matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	set <- function(y) {
      	     x <<- y
                 inv <<- NULL
            }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## This function computes the inverse of the special matrix returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.
## In case of change in matrix value, the inverse is recalculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            matrix <- x$get()
            inv <- solve(matrix, ...)
            x$setinv(inv)
            inv
}

## This function allows to reset/update the base matrix within the special matrix
## created by the makeCacheMatrix function. 
## This is like the update function corresponding to the makeCacheMatrix create function 


updateCacheMatrix <- function(x, y=matrix()){
	  ## Set the matrix to a new value
	  ## Reset the inverse as matrix has changed
	  x$set(y)
}

