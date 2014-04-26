## Coursera R Programming: Programming Assignment 2
##
## The two functions below basically calculate the inverse of a matrix and cache it.
## Specifically, user provides a marix as an input and calculates the inverse. If the
## matrix has been provided in the previous run, the inverse will not be calculated 
## again, rather it'll be retrived from cache.
##
## To test the code, you can run the following:
##
## a<-makeCacheMatrix(matrix(1:4,2,2))
## a$get() 
## b<-cacheSolve(a)
## b
## b<-cacheSolve(a)
## b
##
## a<-makeCacheMatrix(matrix(4:7,2,2))
## b<-cacheSolve(a)
## b


## This function takes a "matrix" as argument. Then it:
## 1.initializes the s variable
## 2.defines the SETter and GETter functions for variables x and s
makeCacheMatrix <- function(x = matrix()) {  ## x variable value is defined here
  
  s <- NULL                 ## initialize s variable
  
  ## SETter function defined within makeCacheMatrix function. Takes a matrix as an argument.
  ## It does not return anything
  set <- function(y) {      
    x <<- y                 ## updates the variable X with the value of y. Operator "<<-" indicates thet the x variable
    ## has already been created before
    s <<- NULL              ## sets variable s to NULL. Operator "<<-" indicates thet the x variable
    ## has already been created before
  }
  ## GETter function. Takes no argument. It returns value of variable x
  get <- function() {
    x
  }
  
  ## SETter function. It takes a matrix as an argument. It does not return anything
  setInverse <- function(inverse) {
    s <<- inverse           ## passes the value of the given argument to variable s. Operator "<<-" indicates thet the x variable
    ## has already been created before
  }
  
  ## GETter function. Takes no argument. It returns value of variable s (which is the inverse matrix)
  getInverse <- function() {
    s
  }
  
  ## list of the functions defined above. The list is accessed when the user runs wants to call any of them
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function takes a "matrix" as argument. Then it checks whether the
## 1.initializes the s variable
## 2.defines the SETter and GETter functions for variables x and s
cacheSolve <- function(x, ...) {
  
  s <- x$getInverse()       ##retrieve the  cachedinverse of the given matrix
  
  if(!is.null(s)) {         ##check whether the inverse is not NULL (has not been calculated before) 
    message("There is cached inverse of the given matrix. Getting cached data...")
    print(s)                ##return the cached inverse of the matrix (no calculation of the inverse needed)
  }
  
  else {
    
    message("There is no cached inverse of the given matrix. Calculating inverse...")
    
    data <- x$get()         ##get the given matrix and pass it into data variable
    
    s <- solve(data, ...)   ##calculate the inverse of the matrix and pass it to variable s
    
    x$setInverse(s)         ##set the inverse of the matrix into variable s
    
    print(s)                ##return s (the inverse of the matrix)
  }
}