library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() {
                            inver <- ginv(x)
                            inver%*%x
  }
  list(set = set, get = get, 
       setinv = setInv, 
       getinv = getInv)
}
## write a short comment describing this function
## This is used to get the cache data
cacheSolve <- function(x, ...) 
  {
  inv <- x$getinv()               ##checking whether inverse is Null
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)     ##returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv     ## Return a matrix that is the inverse 
}
