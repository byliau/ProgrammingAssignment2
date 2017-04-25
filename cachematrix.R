## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y ## use `<<-` to assign a value to an object in an environment different from the current environment. 
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # if the inverse has already been calculated
  if (!is.null(inv)){
    
    message("getting cached data") #get it from the cache and skips the computation.
    return(inv)
  }
  
  
  mat.data <- x$get() # otherwise, calculates the inverse
  inv <- solve(mat.data, ...)
  
  
  x$setinv(inv)# sets the value of the inverse in the cache via the setinv function.
  
  return(inv)
}
