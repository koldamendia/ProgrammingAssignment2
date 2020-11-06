##These functions store the inverse of a matrix in the cache and then calls 
##the result up if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
  #This functions calculates and returns the inverse matrix of 'X'
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <-- NULL
  }
  get <- function()x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x' 
        ## either calling the cached value or calculating it if not cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
