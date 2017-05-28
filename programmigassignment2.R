makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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

##m <- matrix(rnorm(16),4,4)
##m1 <- makeCacheMatrix(m)
##cacheSolve(m1)
##
##[,1]        [,2]        [,3]       [,4]
##[1,]  0.4261289  0.13332018  0.10340774  0.1699766
##[2,]  0.1521815  0.03168742 -0.47774214 -0.3440438
##[3,]  0.3737353 -0.21044730  0.22575096 -0.4803671
##[4,] -0.6094139  1.07047870 -0.06841115 -1.4438538