## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # the inverse
  inv <- NULL

  # sets x (the matrix) and empties the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # returns the matrix
  get <- function() x

  # sets the inverse (caching)
  setinverse <- function(inverse) inv <<- inverse

  # getter function returns the inverse
  getinverse <- function() inv

  # returns a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix().
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  # tries to get an inverse matrix if already cached
  # in the "special" matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # the inverse exists, return it
    message("getting cached data")
    return(inv)
  }

  # the inverse doesn't exist, so get the matrix
  # data from the special matrix object
  data <- x$get()

  # compute the inverse matrix
  inv <- solve(data, ...)

  # cache the inverse matrix into the special matrix
  # for next time
  x$setinverse(inv)

  # return the result
  inv

}
