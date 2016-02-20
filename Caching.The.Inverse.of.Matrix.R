
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y    # Set the value
        m <<- NULL # Clear the cache
    }
   get <- function() x
    # Define function to set the inverse. This is only used by getinverse() when
    # there is no cached inverse
    setInverse <- function(inverse) m <<- inverse
    # Define function to get the inverse
    getInverse <- function() m
    
    # Return a list with the above four functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x) {
    m <- x$getInverse() # This fetches the cached value for the inverse
    if(!is.null(m)) { # If the cache was not empty, we can just return it
        message("getting cached data")
        return(m)
    }
    # The cache was empty. We need to calculate it, cache it, and then return it.
    data <- x$get()  # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setInverse(m)  # Cache the result
    m                # Return the inverse
}

