##Create special matrix

makeCacheMatrix <- function(x = matrix()) {
  ##set j to NULL as placeholder
  j <- NULL 
  #defines function to set vector x to new vector y and resets matrix j to NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  ##returns vector x
  get <- function()x
  ##sets inverse to j 
  setInverse <- function(inverse) j <<- inverse
  ##returns j
  getInverse <- function() j 
  ## returns the 'special vector' containing all of the functions just defined
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##code check if j is valid. If j exist, skip calculation and use the cache variable.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}