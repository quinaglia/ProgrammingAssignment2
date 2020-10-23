## This function will create an object which is capable of storing a matrix and 
## and caching the inverse of this matrix

## it sets and gets the values of a matrix
## then it calculates and stores the inverse of this same matrix

makeCacheMatrix <- function(x = matrix()) {
## i is first set as an empty object as a place holder
    i <- NULL
  set <- function(y) {
    ## y is set to assign a value to x in this environment 
    x <<- y
    i <<- NULL
  }
  ## the matrix will be obtained
  get <- function() x
  ## and then its inverse determined and stored
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will use the function solve the matrix x by calculating its
## inverse i but only when the result is not cached already by the previous
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## if i was already calculated in the previous function a message will show the
  ## the cached data will be used for i
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if i was not calculated and cached this function will calculate the inverse
  ## of matrix x
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
