## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This code contains two functions makeCacheMatrix which makes 
##the special "matrix" and then finds the solution(inverse) and 
##stores it and cacheSolve which either returns the inverse 
##already stored or finds the new solution (inverse). Inside 
##these functions are functions get,set,setmatrix and 
##getmatrix. When getmatrix is called from cacheSolve, 
##if the matrix is already computed, the cached solution is returned 
##else it calculates the inverse and returns as solution

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<-solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
  if(!is.null(m)){
   message("getting cached data")
    return(m)
  }
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m}
