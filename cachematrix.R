##course: R programming, assignment 2 week 3
##inverting a large matrix is time consuming
##the aim of the assignment is to write a pair
##of functions that cache the inverse
##of a matrix
##makeCacheMatrix: this function creates a 
##special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##cacheSolve: this function computes the inverse 
##of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been caculated,
##then the cacheSolve should retrieve the inverse
##from the cache
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setsolve(m)
  m
}