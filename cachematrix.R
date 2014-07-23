## This file contains two functions: makeCacheMatrix and cacheSolve.
## The functions can be used to create a special matrix object which can be used to cache result of the expensive solve function.

## The makeCacheMatrix function creates an special matrix object based on the matrix of parameter x.
## The object can be used to get and set the value of the matrix and get and set the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## The cacheSolve function returns the inverse of 'x'. If the cached result of the solve function exists,
## then it returns the cached value. Otherwise, it calculates the inverse and stores it in the object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")  
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
