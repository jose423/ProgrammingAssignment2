## makeCacheMatrix - store cached inverse of matrix
## cacheSolve - retrive cached inverse of matrix if available.
##              Otherwise, calculate and chache inverse of Matrix.
## 

## Stores Matrix data and cached inverse of Matrix

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


## Looks up cached inverse of Matrix and returns it. If cached is
## null, calculate inverse of Matrix, cache it, and return it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cahced data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
