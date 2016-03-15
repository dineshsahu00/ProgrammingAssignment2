# Function makeCacheMatrix looks for the cache
# And Define the getters and setters 

makeCacheMatrix <- function(x = matrix()) {
  if(!exists("m")){
    m=NULL
  }
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function()x
  
  setInverse <- function(m){
    m<<-m
  } 
  getInverse <- function()m
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Function looks into the cache first, if found returns the inverse
## If not found in the cache computes the inverse and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  t<-makeCacheMatrix(x)
  m <- t$getInverse()
  if(!is.null(m)){
    print("Getting cached data")
    return(m)
  }
  data<- t$get()
  m<- solve(data)
  t$setInverse(m)
  m<<-m
  m
}
