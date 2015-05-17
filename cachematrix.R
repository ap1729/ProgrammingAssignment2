## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMAtrix creates a list of functions which can set and retrieve
## the matrix x and its inverse xinv
##intially xinv is set to NULL
makeCacheMatrix <- function(x = matrix()) {
 
  ndim<-nrow(x)
  xinv<-matrix(nrow=ndim,ncol=ndim)
  xinv<-NULL
  set<-function(z)
      {
      x<<-z
      xinv<<-NULL
      }
   get<-function() x
  setinv<-function(y) xinv<<-y
  getinv<-function() xinv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##cacheSolve first checks if the inverse has already been calculated.
##If it has been calculated(not NULL) then it prints a message saying "RETRIEVING CACHE DATA" and
## returns the already eexisting value of xinv
##then the value of matrix x is assigned to matrix1 using x$get()
##xinv is calculated  using xinv. If xinv already exists it will be storing the same data again
##xinv is then printed out
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        xinv<-x$getinv()
   if(!is.null(xinv))
    {
      message("RETRIEVING CACHE DATA")
      return(xinv)
    }
  matrix1<-x$get()
  xinv<-solve(matrix1)
  x$setinv(xinv)
  xinv
}
