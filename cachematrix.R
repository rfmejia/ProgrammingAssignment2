# A function wrapper to a matrix that caches its the computation of its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  # Cached computation of inverse
  inv <- NULL

  get <- function() mat
  get.inverse <- function() inv  
  
  # Changes the supplied matrix and resets the computation value in the
  # parent environment
  set <- function(newMat) {
    mat <<- newMat
    inv <<- NULL
  }

  # Set the computation value in the parent environment
  set.inverse <- function(newInv) inv <<- newInv

  list(
    get = get,
    get.inverse = get.inverse,
    set = set, 
    set.inverse = set.inverse
  )
}

# Given an instance of the makeCacheMatrix() function wrapper and *optional* 
# argument(s), execute the solve() function on the cached matrix with the 
# supplied argument(s) to compute for its inverse and cache into the instance
cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  
  # Return the cached inverse computation, if it exists
  if(!is.null(inv)) inv
  else { # If it does not exist, compute the inverse and cache before returning
    mat <- x$get()
    inv <- solve(mat, ...) # Pass the supplied optional arguments
    x$set.inverse(inv)
    inv
  }
}
