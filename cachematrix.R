## makeCacheMatrix creates a list of functions that act together to store
## information about a matrix. It is like a matrix with special caching 
## functionality that is accessed as objectname$get. cacheSolve uses its
## functionality intelligently, checking if the object already has the inverse
## of the matrix stored, and calculates it if not. 

## this function creates the "special matrix" list of functions

makeCacheMatrix <- function(x = matrix()) {
  store_inverse <- NULL ## this variable can store the inverse of the matrix
  ## this function gives the matrix a new value and resets store_inverse,
  ## since if there was an inverse already stored it will no longer be correct
  set <- function(y) {
    x <<- y
    store_inverse <<- NULL
  } 
  ## this function is what lets you access the matrix
  get <- function() x
  ## this simple function takes in an already calculated inverse and stores it
  setinverse <- function(inverse) store_inverse <<- inverse
  ## this function lets you access the inverse once stored, or NULL if not
  getinverse <- function() store_inverse
  ## makeCacheMatrix returns a list of the above-defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the matrix corresponding to a list created by 
## makeCacheMatrix and stores the inverse, or returns it if already stored

cacheSolve <- function(x, ...) {
  ## first look at what's currently stored in x
  store_inverse <- x$getinverse()
  ## if it isn't null, then it has already been calculated, and can be returned
  if(!is.null(store_inverse)) {
    message("getting cached data")
    return(store_inverse)
  }
  ## no 'else' is needed; the if ends in a return, so if we're here it's time to
  ## solve the matrix and store the inverse before returning it
  data <- x$get()
  my_inverse <- solve(data, ...)
  x$setinverse(my_inverse)
  my_inverse
}
