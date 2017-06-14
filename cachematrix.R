## Put comments here that give an overall description of what your
## functions do

## Comments: function takes a matrix as an argument. Then creates another 
            #blank object "inv" which is kept for later. The subfunction 
            #is used to reset the variables in the parent environment so
            #that each time the parentfunction is run, it will reset instead
            #of using prior arguments. x is not defined in get function 
            #so R retrieves it from the parent environment. m is defined in 
            #the parent environment and we need to access it after the setInverse 
            #runs so the code uses <<- to assign the input argument to the 
            #value of m in the parent environment. Everything is then 
            #returned as a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Comments: function starts with single argument "x" but allows additional
            #arguments to be passed through. the data is retrieved form the
            #object passed in the argument & checks if it is NULL. If yes,
            #then it is returned to parent environment. If no, then  it 
            #calculates inverse with setInverse funtion and returns it to the
            #the parent environment.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting Cached Data Now")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
