makeCacheMatrix <- function(x = matrix()) {
  # x is defined as a matrix
  
  inv = NULL # first, set inv = Null (till it is cached the first time)
  set = function(y,z,r) {
     x <<- matrix(c(y),ncol=z,nrow=r) # set the values of the elemnents in matrix x and its no. of columns and rows 
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    # inverted has already been computed,  get it from the cache. 
    message("getting cached data")
  } else {
    # inverted has not been computed yet, calculate it. 
    message("calculating the inverse ")
  M.data = x$get()
  inv = solve(M.data, ...)
  
  # sets the value of the inverse in the cache.
  x$setinv(inv)
  }
return(inv)
}

# CLARIFYING EXAMPLE

# define the matrix (e.g. 3x3)

y <- c(5, 1, 0,
       3,-1, 2,
       4, 0,-1)
z <- 3
r <- 3

# use the functions  
M <- makeCacheMatrix(x=matrix())
M$get()               # retrieve the value of x
M$getinv()           # retrieve the value of m, which should be NULL

M$set(y,z,r)    # reset value with a new vector
cacheSolve(M)          # calculate the value of the inverse  

#check : multiply a matrix times its invers, get identity matrix

A <- matrix(c(y),z,r)
Ainv <- cacheSolve(M)  # use the value of the inverse that now has been cached        
A %*% Ainv
