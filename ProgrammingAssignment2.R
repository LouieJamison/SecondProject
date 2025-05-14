makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  set <- function(y) {  # Function to set the matrix
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix changes
  }
  get <- function() x  # Function to get the matrix
  setinverse <- function(inverse) inv <<- inverse  # Function to set the inverse
  getinverse <- function() inv  # Function to get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  # Return a list of functions
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if the inverse is cached
  if (!is.null(inv)) {  # If inverse exists, return the cached value
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  # Otherwise, get the matrix
  inv <- solve(data, ...)  # Compute the inverse using solve()
  x$setinverse(inv)  # Cache the inverse
  inv  # Return the inverse
}