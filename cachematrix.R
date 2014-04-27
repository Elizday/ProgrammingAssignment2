## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(A = matrix()) {
    inv  <- NULL
    set  <- function(y){
      A <<- y
      inv <<- NULL 
    }
    get  <- function() A
    setinverse  <- function(inverse) inv  <<- inverse
    getinverse  <- function() inv
    list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'A'
  inv  <- A$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data  <- A$get()
  inv  <- solve(data, ...)
  A$setinverse(inv)
  inv
}