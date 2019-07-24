## First function - makeCacheMatrix takes NULL matrix as an argument and 'fills' it 
## by creating a matrix which can cache its inverse. Furthermore , second function computes 
## the inverse of the invertible matrix but additionally , it checks whether the inverse of this 
## matrix has alredy been computed or not. If it is , then it returns the inverse from cache,
## if not , it computes the inverse of input matrix by using solve() function.

## function basicly contains 4 sub-functions and returns a list that contains all those 4 
## subfunctions. We can think of this function as collection of actions which will be committed 
## depending on matrix.

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(x){
    m <<- x
    inverse <<- NULL
  }
  get <- function(){
    return(m)
  }
  setinverse <- function(y){
    inverse <<- y
  }
  getinverse <- function(){
    return(inverse)
  }
  
  list("set"=set,"setinverse"=setinverse,"get"=get,"getinverse"=getinverse)

}


## Function computes or retrieves inverse of the matrix depending on whether matrix has been 
## taken before or not. It takes matrix as an argument and returns inverse of it. 

cacheSolve <- function(m, ...) {
  inverse <- m$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinverse(inverse)
  inverse
      
}

