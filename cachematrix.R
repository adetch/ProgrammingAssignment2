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
    message("retrieving cached matrix")
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
  print(bogusInverse)
  checkMatrix$setInverse(bogusInverse)
  
  # Test that getInverse returns our bogus inverse matrix
  message("let's use getInverse() and verify it returns the bogus inverse")
  print(checkMatrix$getInverse())
  
  # Test that cacheSolve returns the 'cached' bougs inverse matrix
  message("...and that calling cacheSolve() happily returns the 'cached' bogus inverse as well")
  print(cacheSolve(checkMatrix))
  
  # Let's recreate a fresh matrix
  message("now let's create a shiny new matrix")
  checkMatrix <- makeCacheMatrix(matrix(rnorm(25),5,5))
  print(checkMatrix$get())
  
  # Let's compute the inverse
  message("for which we'll compute the inverse")
  print(inverse <- cacheSolve(checkMatrix))
  
  # Run again and see if we see "getting cached matrix" message
  message("let's see if we get the 'retrieving cached matrix' message when we rerun cacheSolve")
  print(inverse <- cacheSolve(checkMatrix))
  
  # Now we'll check that it multiplying it by the original matrix gives us the identity matrix
  message("this should give us an identity matrix...")
  round(inverse %*% checkMatrix$get())
}