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
makeCacheMatrix <- function(x = matrix()) 
{ 
   
  m <- NULL 
  set <- function(y) 
  { 
   
    global_x <<- y 
    global_m <<- NULL 
  } 
 
  get <- function() return(global_x) 
  set_global_m <- function(m) global_m <<- m 
  get_global_m <- function() return(global_m) 
  list(set = set, get = get, set_global_m = set_global_m, get_global_m = get_global_m) 
} 



cacheSolve <- function(x) 
{ 
  # try to get the value from the global environment. 
  m<- x$get_global_m() 
  if(!is.null(m)) 
  { 
    message("getting cached data") 
    return(m) 
  } 
  data <- x$get() 
  inverseMatrix <- solve(data) 
  x$set_global_m(inverseMatrix) 
  return(inverseMatrix) 
}

