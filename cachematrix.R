## makeCacheMatrix: This function creates a special matrix object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

## This function will create a squareinvertible mtrix
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y) {
    x<<-y
    inv<<-NULL
  }
  get=function()x
  setinv=function(inverse) inv<<-inverse
  getinv=function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function assumes that the matrix is always invertible
## First cheks if the inverse has alredy been computed
## If not, it compute the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv=x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
