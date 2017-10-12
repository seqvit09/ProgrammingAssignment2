## This is a function for 2nd assignment of R course in Coursera


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(ma = matrix()) {
  m <- NULL
  setMat <- function(y) {
    ma <<- y
    m <<- NULL
  }
  getMat <- function() ma
  setInvMat <- function(solve) m <<- solve
  getInvMat <- function() m
  list(setMat = setMat, getMat = getMat,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(ma, ...) {
  m <- ma$getInvMat()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- ma$getMat()
  m <- solve(data,...)
  ma$setInvMat(m)
  m
}
