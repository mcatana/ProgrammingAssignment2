## Store matrix and cache its inverse.
## Get and set value for matrix.
## Get and set value for inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getmatrix<- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the matrix created above with makeCacheMatrix.
## First, check if the inverse has been calculated, if so getinverse returns
## the value from cache, otherwise compute inverse and set it in cache using
## setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }else{
    message("compute and cache inverse")
  }
  minfo <- x$getmatrix()
  m <- solve(minfo, ...)
  x$setinverse(m)
  m
}
