## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts a matrix as input, and returns a list providing four options, to view the matrix, define the matrix, view the inverse, and define the inverse. 

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
  


## CacheSolve checks whether inverse exists for the matrix,  if yes, it returns the cached inverse, else calculates inverse and returns it at the end of function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
