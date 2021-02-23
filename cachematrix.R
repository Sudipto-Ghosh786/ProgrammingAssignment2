
# Considering the matrix supplied here is invertible matrix
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  
  setinverse <- function(inverse) { inv <<- inverse }

  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# Computing and caching the inverse of a matrix

# Here x the result of a previous makeCacheMatrix call and ... is additional arguments 
# Function first checks if inverse is computed or not, if comupter
# it gets the result and returns else it comuptes the inverse and
# sets the value in the cache using setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()   ## This line returns a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}