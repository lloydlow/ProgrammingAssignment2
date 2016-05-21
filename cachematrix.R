## Caching the Inverse of a Matrix.
## Matrix inversion can be a slow process especially when many such computation is 
## needed. One possible way to speed things up is to cache the inverse of a matrix
## and then check in subsequent computation whether the same matrix is already seen,
## which if yes then the cached inverse is returned instead of calculating it.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  #set new value of the matrix if needed
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse matrix
  setInverse <- function(Inverse) I <<- Inverse
  #get the value of the inverse matrix
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then this function retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  dataMatrix <- x$get()
  I <- solve(dataMatrix, ...)
  x$setInverse(I)
  I
}
