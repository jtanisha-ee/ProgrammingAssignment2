## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   #set inv as NULL as it will hold the matrix inverse value
  set <- function(y) {           
    x <<- y                     
    inv <<- NULL                
  }
  get <- function() x                     ## this function will return the value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## this will set the inv value in a parent environment
  getinverse <- function() inv                     ## this will get the value of inv when called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
