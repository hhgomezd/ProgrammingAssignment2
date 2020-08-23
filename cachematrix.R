## We first create a function which creates a special matrix object that
## can cache its inverse. Then we create a function that computes the
## inverse of the special matrix of the first function. 

## In this first function we use the <<- operator in order to create
## a special object that stores a matrix which is a list that sets
## the matrix, then gets the matrix, then sets the inverse of the matrix
## and lastly gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get=get,
       setinverse =setinverse,
       getinverse = getinverse)

}


## This second function calculates the inverse of the special matrix
## It first checks if the inverse has already been calculated, if so
## it skips the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
