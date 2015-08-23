## makeCacheMatrix function create a special "matrix",which has four functions inside
## it. It can set the value of a matrix , get the value of a matrix ,set the value of
## a inverse and get it.And what the cacheSolve function does is to calculate the inverse 
## a matrix if there did't exsit one.

## This function create a special "matrix".It has four functions which i have already 
##described above.

makeCacheMatrix <- function(x = matrix()) {
  r<-NULL
  ## set function can help me to initialize a new matrix as well as give value to a matrix
  set<-function(y){
    x<<-y
    r<<-NULL
  }
  ##get function just help me to get a matrix
  get<-function()x
  ## setreverse function can help me to set a value to the matrix's reverse
  setreverse<-function(reverse)r<<-reverse
  ##getreverse function help me to get a matrix's reverse
  getreverse<-function()r
  ##this makeCacheMatrix function return a list of 4
  list(set=set,get=get,
       setreverse=setreverse,
       getreverse=getreverse)
  
}


## cacheSolve function help me to calculate the matrix inverse if it have't been calculated.
## it return the matrix inverse
cacheSolve <- function(x, ...) {
  ##we first get the value of inverse from the makeCacheMatrix.
  r<-x$getreverse()
  ##if the reverse have existed in the makeCacheMatrix function, it just return that reverse
  if(!is.null(r)){
    message("getting cached data")
    return(r)
  }
  ##if it does't exist ,we just calculate
  data<-x$get()
  r<-solve(data)
  x$setreverse(r)
  r
}