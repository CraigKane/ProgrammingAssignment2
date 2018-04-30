## This script creates two functions which calculate the inverse of a matrix and cache the result for later reference.

## The first function below, makeCacheMatrix, coerces the input data into a matrix and then creates a list of 4 functions with the matrix.
## which are then used by the second function below (cacheSolve)

## Within makeCacheMatrix the 4 functions do the following:
##         set is a function which assigns the matrix to variable x and sets m to null
##         get is a function which pulls variable x (from set above)
##         setinverse is a function which inverts the matrix
##         getinverse is a function which retrieves the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
  x <<- y
  m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is the second function which uses the list of functions within makeCacheMatrix along with logic to determine whether the inverse is already cached
## or if it needs to be calculated.  If it is already cachged then it returns the cached matrix but if it is not then it returns the computed inverse.
  
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        
}

## the script below can be used to test the script above.

## x <- cbind(c(1,3),c(1,5))   create data to be inverted
## xx <- makecachematrix(x)    creates list of functions utilizing 'x' 
## cachesolve(xx)              Return a matrix that is the inverse of 'x'
## cachesolve(xx)              Entering this a second time will return the cached inverse of 'x'
