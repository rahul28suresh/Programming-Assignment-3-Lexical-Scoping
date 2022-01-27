## Put comments here that give an overall description of what your
## functions do

#Will return the inverse of a matrix inputed into makeCacheMatrix.
#The input from makeCacheMatrix will then be solved in cachesolve

#Creates a matrix obeject that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<- solve
  getsolve <- function() m #will now create list will all the required functions
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve= getsolve)
}


#finds the inverse of the matrix if m=NULL.  

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m))  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m) #stores inverse of matrix to m
  m
}
