## Functions to cache the inverse of a matrix (rather than compute it repeatedly) and return it

# Function #1
## sets the value of the matrix, gets the value of the matrix
## sets the value of the matrix inverse, gets the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse =  getinverse)
}


#  Function #2
## Computes the inverse of the matrix returned by makeCacheMatrix. Will retrieve the inverse from
## cache if it has already been calculated.

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
