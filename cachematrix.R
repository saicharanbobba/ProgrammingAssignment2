## matrix inverse Calculation  is a complex computation and hence calculating its value repeatedly is a 
## long process so its better to cache the inverse of a matrix rather than calculating it
## repeatedly.

## 
## this function creates a special matrix object that can cache its inverse value

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


## Write a short comment describing this function
## this function calculates the inverse of special matrix returned by makecache matrix 
## above . IF inverse had already been calculated and no change has occured to the matrix
## then cachesolve should retrieve inverse value from cache 
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
        ## Return a matrix that is the inverse of 'x'
} 
