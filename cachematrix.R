## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ##set the value of the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
    
    
  }
  
  ##get the value of the matrix
  
  get <- function() x
  
  ##set the inverse of the matrix
  
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  
  ##get the inverse of the matrix
  getInverse <- function() inverse
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse))
  {
    return(inverse)
  }
 
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
  
  
}
