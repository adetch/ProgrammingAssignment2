## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  
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
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting inverted matrix")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  return(inverse)
}

checkWork <- function() {
  # Creates matrix to work with
  checkMatrix <- makeCacheMatrix(matrix(rnorm(25),5,5))
  message("here's our brand new matrix!")
  print(checkMatrix$get())
  
  # Creates a bogus 'inverse'
  bogusInverse <- matrix(rep(1,25),5,5)
  message("let's create a bogus inverse matrix and assign it using setInverse")
  checkMatrix$setInverse(bogusInverse)
  
  # Test that getInverse returns our bogus inverse matrix
  message("let's use getInverse() and verify it returns the bogus inverse")
  print(checkMatrix$getInverse())
  
  # Test that cacheSolve returns the 'cached' bougs inverse matrix
  message("and calling cacheSolve() gives us the bogus matrix as well")
  cacheSolve(checkMatrix)
  
  # Let's recreate a fresh matrix
  message("now let's create a new matrix")
  checkMatrix <- makeCacheMatrix(matrix(rnorm(25),5,5))
  checkMatrix$get()
  
  # Let's compute the inverse
  message("computing the inverse")
  print(inverse <- cacheSolve(checkMatrix))
  
  # And check that it multiplying it by the original matrix gives us the identity matrix
  message("this should give us an identity matrix...")
  round(inverse %*% checkMatrix$get())
}