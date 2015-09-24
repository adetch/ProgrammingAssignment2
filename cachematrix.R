## matrix inversion caching exercise

## Create matrix interface, instantiating getters & setters

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(value) inverse <<- value
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

## Returns cached inverse matrix, or calculate it if no cache present

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

## Walk through proof of work

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
  
  # Test that cacheSolve returns the 'cached' bogus inverse matrix
  message("...and that calling cacheSolve() happily returns the 'cached' bogus inverse as well")
  print(cacheSolve(checkMatrix))
  
  # Let's recreate a fresh matrix
  message("now let's create a shiny new matrix")
  checkMatrix <- makeCacheMatrix(matrix(rnorm(25),5,5))
  print(checkMatrix$get())
  
  # Let's compute the inverse
  message("for which we'll compute the inverse")
  print(inverse <- cacheSolve(checkMatrix))
  
  # Run again and see if we see "retrieving cached matrix" message
  message("let's see if we get the 'retrieving cached matrix' message when we rerun cacheSolve")
  print(inverse <- cacheSolve(checkMatrix))
  
  # Now we'll check that it multiplying it by the original matrix gives us the identity matrix
  message("now we'll check that we actually have the inverse. \nmultiplying the two should give us an identity matrix...")
  round(inverse %*% checkMatrix$get())
}