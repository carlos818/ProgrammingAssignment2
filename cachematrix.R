## Programming Assingment #2 by Carlos De Los Santos
## Purpose of the assignment was to make a Matrix and then calculate it's inverse and return it
## instanteneously if it was solved already
## This assigns a method for cacheSolve to find if the Matrix was solved in the past
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
## cacheSolve checks to see if the inverse of the "matrix" was calculated.
## If it exists in the memory it outputs it instantly
## If not it saves it to the memory for later use
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("accessing cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
