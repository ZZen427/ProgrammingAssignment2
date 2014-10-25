## makeCacheMatrix() and cacheSolve() works together to cache and compute 
## the inverse of a matrix, respectively
## Assuming supplied matrix is always invertible

## makeCacheMatrix() is implemented as a generic caching fucntion of any object
## makeCacheMatrix() takes an object (e.g. matrix) as argument and returns a 
## list of functions that retrieve and modify the input object and the cached
## object. 
## Object to which the return of makeCacheMatrix() assigned also stores
## variable "cache" in its environment to be used later on.

makeCacheMatrix <- function(x = matrix()) {
  # Force the evaluation of function argument "x" b/c R uses lazy evaluaton 
  # for function arguments by default, which means variable are only evaluated 
  # when using. 
  # force() is a useful defensive meseaure if the argument will 
  # be captured in a closure by the lexical scoping rules and will later be 
  # altered by an explicit assignment or an implicit assignment in a loop or 
  # an apply function
  force(x)
  # Initialize "cache" variable to NULL object
  cache <- NULL
  # Returns a list of functions to be used in cacheSolve()
  list(
       set = function(y) {
         x <<- y
         cache <<- NULL
       },
       get = function() {
         x
       },
       # Superassign "cache" with variable "object", any R object can be cached 
       setcache = function(object) {
         cache <<- object
       },
       getcache = function() {
         cache
       }
  )
}

## cacheSolve() takes the cache object returned from makeCacheMatrix() 
## as its argument and returns the inverse of the matrix in the input object.
## Returns an error message if input object does not contain a matrix

cacheSolve <- function(x, ...) {
  cache <- x$getcache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  # Terminate program if x is not a matrix b/c solve() only works w/ matrix
  if(!is.matrix(data)) stop("input must be a matrix")
  # compute inverse of a matrix then returns it
  cache <- solve(data, ...)
  x$setcache(cache)
  return(cache)
}
