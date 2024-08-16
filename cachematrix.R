## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.
# Define makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  set <- function(y) {
    x <<- y       # Set the matrix
    inv <<- NULL  # Reset the inverse cache since the matrix has changed
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getInverse <- function() inv  # Return the cached inverse
  
  # Return a list of all the functions to interact with the matrix and inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}

}
