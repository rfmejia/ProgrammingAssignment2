# cachematrix.R - Set of functions that take advantage of function wrapper
# instances and manipulation of variables in a parent environment to cache a
# matrix and the computation of its inverse.

## Note 1: Assumes that all supplied matrices are invertible (as per specs).

## Note 2: function naming (including ones within makeCacheMatrix()) are patterned
## against the unit test found in (link)[post-336] just in case this was part of
## the testing requirements.
## [post-336][https://class.coursera.org/rprog-007/forum/thread?thread_id=83#post-336]


# A function wrapper to a matrix that caches its the computation of its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  # Cached computation of inverse
  inv <- NULL

  get <- function() mat
  getinverse <- function() inv  
  
  # Changes the supplied matrix and resets the computation value in the
  # parent environment
  set <- function(newMat) {
    mat <<- newMat
    inv <<- NULL
  }

  # Set the computation value in the parent environment
  setinverse <- function(newInv) inv <<- newInv

  list(
    get = get,
    getinverse = getinverse,
    set = set, 
    setinverse = setinverse
  )
}


# Given an instance of the makeCacheMatrix() function wrapper and *optional* 
# argument(s), execute the solve() function on the cached matrix with the 
# supplied argument(s) to compute for its inverse and cache into the instance
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # Return the cached inverse computation, if it exists
  if(!is.null(inv)) {
    print("getting cached data")
    inv
  }
  else { # If it does not exist, compute the inverse and cache before returning
    mat <- x$get()
    inv <- solve(mat, ...) # Pass the supplied optional arguments
    x$setinverse(inv)
    inv
  }
}
