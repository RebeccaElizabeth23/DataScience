# Function to create an cache matrix to store inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initially the inverse is a NULL value
  inv <<- NULL
  # Set the cached matrix to the function argument x
  set <- function(y) {
    x <<- y
    # Change value of matrix inverse in case the matrix was altered
    inv <<- NULL
  }
  
  # Gets the inverse value
  get <- function() x
  
  # Calculates matrix inverse using the solve function
  setinverse <- function(inverse) inv <<- inverse
  # Get the matrix inverse
  getinverse <- function() inv
  
  # Passes  function values to makeCacheMatrix        
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# Function to check if caches inverse exists and calculate if none found
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # Check if there is a cache file; return and stop recalculation
  if(!is.null(inv)) {
    message("Getting cached data - Inverse found")
    return(inv)
  }
  # If no cache, recalculate matrix inverse (pass back to Setinverse)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  # Display inverse
  inv 
}
  
  