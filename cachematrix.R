## Johns Hopkins University : Data Science 
## COurse 2 : R Programming
## Peer Graded Assignment
## Atharva Ramgirkar
## github username : gato1005

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.mat<-NULL
  set <-function(y){
    x<<-y
    inv.mat<<-NULL
  }
  
  get <-function() x
  
  set.inv<-function(inverse) inv.mat<<-inverse
  get.inv<-function() inv.mat
  
  list(set=set,
       get=get,
       set.inv=set.inv,
       get.inv=get.inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv.mat<-x$get.inv()
  if(!is.null(inv.mat)) {
    message("getting cached data")
    return(inv.mat)
  }
  data <- x$get()
  inv.mat <- solve(data, ...)
  x$set.inv(inv.mat)
  inv.mat
}
