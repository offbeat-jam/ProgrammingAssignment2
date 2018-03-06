## The two functions together get the matrix and then retrieve the value of its inverse
## If the inverse has already been calculated once , its value is cached instead of calculating it
## repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #sets the value of x and inv in parent environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #retrieves the matrix
  get <- function() x
  
  #sets the value of inverse in parent environment
  setinverse <- function(inverse) inv <<- inverse
  
  #retrieves the value of the inverse
  getinverse <- function() inv
  
  #returns a list with all the getter and setter functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above.

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
