## Functionality for caching the inverse of a matrix

## Function for creating a special 'matrix' object that can 
## cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    # initiate an object for storing the inverse
    inv <- NULL 
  
    # function for setting the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  
    # function for retrieving the matrix
    get <- function() x
  
    # function for setting the inverse
    # and saving to cache
    setinv <- function(solve) inv <<- solve
    
    # function for retrieving the inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function for computing the inverse of the special 'matrix'
## object retunred by the 'cacheMatrix' function above. If the
## inverse has already been calculated (and the matrix is
## unchanged), the function retrives the inverse from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    # retrive the inverse
    inv <- x$getinv()
  
    # if inverse is not null it is returned from cache
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  
    # if we reached here, get the matrix,
    # compute it's inverse and save it to cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)

    inv
}
