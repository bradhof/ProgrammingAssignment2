####################################################################
# makeCacheMatrix function
# parameter x: a reference to a square matrix object (2x2, 3x3, etc)
# return: an object representing the matrix with four methods
# set():        sets a new matrix to the object and resets the inverse matrix
# get():        gets the matrix 
# setInverse(): stores the inverse of the matrix
# getInverse(): gets the inverse of the matrix (if calucated using cacheStore())
####################################################################

makeCacheMatrix <- function(x = matrix()) {
     #initialize the inverse matrix to NULL
     inverseMatrix <- NULL
     
     #sets the internal matrix object and resets the inverse to NULL
     #so it gets re-calculated
     set <- function(y) {
          x <<- y
          inverseMatrix <<- NULL
     }
     
     #returns the internal matrix object
     get <- function() x
     
     #sets the inverse matrix
     setInverse <- function(inverse) inverseMatrix <<- inverse
     
     #returns the inverse matrix
     getInverse <- function() inverseMatrix
     
     #return the list of functions to the caller
     list(set = set, 
          get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
}


####################################################################
# cacheSolve function
# parameter x: a reference to a makeCacheMatrix object
#         ...: additional solve() parameters
# description: calculates and caches the inverse for a square matrix
####################################################################

cacheSolve <- function(x, ...) {
     #first, attempt to get the inverse from the special matrix object
     inverse <- x$getInverse()
     
     #if the inverse was already cached, return the cached value
     if(!is.null(inverse)) {
          message("getting cached inverse")
          return(inverse)
     }
     
     #otherwise, get the original matrix object, and calculate the inverse
     data <- x$get()
     inverse <- solve(data, ...)
     
     #set/cache the inverse matrix on the object, so it doesn't get recalculated 
     #every time
     x$setInverse(inverse)
     
     #return the inverse
     inverse
     
}
