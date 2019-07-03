## we are going to write a pair of functions "makeCacheMatrix" and "cacheSolve" which compute the inverse of
## a invertible matrix. So below is the explanation how thw functions are formed

## The function "makeCacheMatrix" returns a special "matrix" object that can 
## obtain its inverse for the input which is an invertible square matrix
#return invers matrix without repeat calculation

 makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


 cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
