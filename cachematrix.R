## Programming Assignment 2:
#The first function, makeCachematrix calculate the inverse of matrix,
# which is really a list containing a function to
#set the matrix
#get the matrix
#set the inverse of the matrix
#get the inverse of the matrix
#through solve(x) 

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The function cacheSolve calculates the inverse of the matrix 
#created with the above function. However, it first checks to see 
#if the inverse has already been calculated. If so, it gets the inverse
#from the cache and skips the computation. Otherwise, it calculates 
#the inverse of the data and sets the value of the mean in the cache 
#via the setinverse function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data,...)
  x$setinverse(m)
  m
  
}

# test:
#x <- matrix(rnorm(16),4)