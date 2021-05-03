## our aim is to create two functions that cache the inverse of a matrix
## the following set of code creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( ourmatrix = matrix() ) {
  ## in order to initialize the inverse prop
  initialize <- NULL
  ## the following method is done to set the matrix
  set <- function( matrix ) {
    ourmatrix <<- matrix
    initialize <<- NULL
  }
  ## the following method is to get the matrix
  Get <- function() {
    ## to return the mtrx
    ourmatrix
  }
  ## the following method is to set the inv of the mtrx
  setInverse <- function(inverse) {
    initialize <<- inverse
  }
  ## the following method aims at getting the inv of mtrx
  getInverse <- function() {
    ## Return the inverse property
    initialize
  }
  ## inorder to return the list of methods
  list(set = set, Get = Get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## the above function tries to compute the inv of the mtrx.To obtain the original matrix again or cache the matrix the following set of codes is run. 
cacheSolve <- function(x, ...) {
  ## we now obtain a matrix that is inverse of x
  ourmatrix <- x$getInverse()
  ## to return the iverse if alrdy set
  if( !is.null(ourmatrix) ) {
    message("getting cached data")
    return(ourmatrix)
  }
  ##inorder to obtain the object from thbe method
  data <- x$Get()
  ## using matrx multipliaction to obtainthe inv
  ourmatrix <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(ourmatrix)
  ## to return the matrix
  ourmatrix
}
