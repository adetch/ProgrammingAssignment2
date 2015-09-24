## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get function() x
  getInverse <- function(inv)
  setInverse <- function(solve) inv <<- solve
  
  return(
    list(
      set=set,
      get=get,
      getInverse=getInverse,
      setInverse=setInverse
      )
    )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse
  if(!is.null(m)) {
    message("getting inverted matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x.setInverse(inverse)
  return(inverse)
}
