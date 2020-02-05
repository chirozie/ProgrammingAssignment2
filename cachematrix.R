##This is a pair of functions that caches the inverse of a matrix


## This is the first function and it will create a special matrix
## This special matrix is a list containing functions 
## to set and get the value of the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This second function computes the inverse of matrix returned by first function above
## If the inverse has already been calculated, this function retrieves it from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
