## This pair of functions extend the built-in solve function by caching the result for later use
## Usage xample:
## x=matrix(runif(4), nrow=2)
## xc = makeCacheMatrix(x)
## cacheSolve(xc) # oops printed to console but I wanted to save the result
## y = cacheSolve(xc) # yeah! no need to resolve: function reports "getting cached data"
## x %*% y # Aye, it's an eye alright

## Creates the structured object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inv <<- solve
  getSolve <- function() inv
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Return a matrix that is the inverse of 'x' either by calling solve() or by retrieving a cached copy if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Input is an object returned by makeCacheMatrix()
  inv <- x$getSolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setSolve(inv)
  inv ## output is a matrix
}