## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Author : Jaymin Barot
  # Purpose:- making a cached matrix.
  # --------------------------------
  # Result set weill be
  #  
  #  1. set matrix
  #  2. get matrix
  #  3. set inverse
  #  4. get inverse
  #  list is used as the input to cacheSolve function
  
  invr = NULL
  set = function(y) {
    
    x <<- y
    invr <<- NULL
  }
  get = function() x
  setinv = function(inverse) invr <<- inverse 
  getinv = function() invr
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # output of makeCacheMatrix
  # results the inverse of the original matrix input to makeCacheMatrix function
  
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data from cache")
    return(inv)
    # take it from cache if inverse found already calculated & returned to makeCacheMatrix
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
  # if inverse NOT found already calculated in cache , then compute  & returned to makeCacheMatrix
}
