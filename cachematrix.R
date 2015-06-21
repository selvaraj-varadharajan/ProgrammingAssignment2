## This function creates the square matrix and computes its inverse
## and store it in the global environment as a list

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y = matrix()) {
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


## This function calculates the inverse of the square matrix 
## only if the inverse hasn't computed already
## Finally it returns a inverse matrix

cacheSolve <- function(x, ...) {
        
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  a <- x$get()
  m <- solve(a, ...)
  x$setsolve(m)
  m
}
