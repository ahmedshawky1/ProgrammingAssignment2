## Functions to do a cache for matrix inverse


## function to cache inv matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invmatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  get <- function() x
  
  setInvMatrix <- function(matrix) invmatrix <<- matrix
  getInvMatrix <- function() invmatrix
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
  
}


## Function that checks if we have a cache version or not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x''
  
  #get the Cache version 
  invmatrix <- x$getInvMatrix()
  
  #check if the Cache version is valid 
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  
  #no cached version we have to get the inv matrix
  data <- x$get()
  invmatrix <- solve(data, ...)
  #set the data for next time to reuse
  x$setInvMatrix(invmatrix)
  invmatrix
  
}