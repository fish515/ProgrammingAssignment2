## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that are used to create a specual object that stores a matrix and cache's its inverse.
## The first function, makecacheMatrix creates a special "matrix", containing following feature:
## set the value of the matrix and get the value of the matrix and then set the value of the inverse matrix and get the value of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special matrix created with the above function.
## First, it checks to see if the inverse has been already been calculated. If so, it gets the inverse from the cache and skips the computation.
## otherwise, it calcuates the inverse matrix of the data and sets the value of the inverse matrix  in the cache via the setinverse funciotn.

cacheSolve <- function(x, ...) {

          ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
