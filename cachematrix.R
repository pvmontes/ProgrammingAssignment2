## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function recives a matrix like an argument; after it stores, in a list, the "setters" and "getters" that the function
#cacheSolve will use. Besides it uses the superassigment operator to stablishes values in the upper enviroment.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#It uses the result of the function makeCacheMatrix and retrieves the inverse of the matrix. I have used the solve function
#for that.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


