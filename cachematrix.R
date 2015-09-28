# These functions allow for creation of a special "matrix" object
# which can cache it's inverse in it's own environment and a function
# that can retrieve the inverse if it exists or calculate and cache it
# it it does not exist.


# makeCacheMatrix():
# Creates a special "matrix" object whose data 
# and inverse can be accessed through "get" or "set" methods.
# The matrix object will set its inverse to NULL upon creation
# or resetting of its internal data.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # set inverse to NULL upon creation
  set <- function (y) {
    x <<- y
    m <<- NULL # set inverse to NULL upon changing internal data
  }
  get <-function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve()
# Takes a "matrix" created by the makeCacheMatrix() function as input
# and checks to see if the inverse of the internal data of the "matrix"
# object exists. If it already exists, that inverse is returned, otherwise
# the inverse of the object's matrix is computed via the solve() function
# and stored into the object. Finally the inverse is returned.
cacheSolve <- function(x, ...) {
  m <- x$getinverse() # try to access inverse of input object
  if(!is.null(m)){ # if the inverse exists, return it
    message("getting cached data")
    return(m)
  }
  # if the inverse did not exist, compute it.
  message("computing inverse since none cached...")
  data <- x$get() 
  m <- solve(data,...)
  x$setinverse(m)
  return(m) ## Return a matrix that is the inverse of 'x'
}


matIn <- matrix(c(1,4,6,3),2,2)
x <- makeCacheMatrix( matIn )
inverse <- cacheSolve( x )
print(inverse)
inverse <- cacheSolve( x )
print(inverse)