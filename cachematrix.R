#The following funtions solve the inverse for a matrix A and stores the result into a cache object. 
#There are two particular functions:
#1.Function makeCacheMatrix
#2.Function CacheSolve

#Function makeCacheMatrix create an object containig a list of functions: set, get, setInv, getInv.
#Its argument is the matrix to be inverted, if its inverse has not been solve previuosy.
#if you want set cacheMat for a particular matrix to be inverted, run:
#A<-matrix(1:4,nrow=2,ncol=2) 2x2 matrix or any matrix
#cacheMat<-makeCacheMatrix(A)
#Function "set" assings to x the value of the matrix A and the initial value of m (by using operator <<-) on the "parent environment".
#Function "get" updates the value of x on cacheMat$get
#Function "setInv" assings to m the value of inverse matrix (to be solved in the second function cacheSolve).
#Note that "setInv" has one argument: the obtained  inverse matrix.
#Function "getInv" updates the value of the inverse on cacheMat$getInv 
#Finally, the function list, creates a list of named values into the environment in which they are evaluated.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


#Function cacheSolve solve the inverse of a matrix previouly setting by running the function "makeCacheMatrix"
#You can run this function by running the code:
#InverseMatrix<- cacheSolve(cacheMat)
#At first, the function checks if there is an inverse matrix updated on x$getInv() and return (leave this function) this inverse matrix.
#In this case you can see the message "getting cached data"
#Else, if there is no an inverse matrix stored in x$getInv(), it proceeds to solve the inverse for the matrix in x$get().
#Finally, this inverse matrix is updated on x$setInv
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matInv <- x$get()
  m <- solve(matInv)
  x$setInv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

#You can run the code:
#source("makeMatrix.R")
#source("solveMatrix.R")

#A<-matrix(1:4,nrow=2,ncol=2) #set the matrix to be inverted (be sure it is invertible)
#cacheMat<-makeCacheMatrix(A)
#InverseMatrix<-cacheSolve(cacheMat)
#identity<-A%*%InverseMatrix #proof ok

#Note that each time you run InverseMatrix<-cacheSolve(cacheMat), the function return the inverse of the matrix in cacheMat$get().
#In this case you see the message "getting cached data".
#If you can set again a new matriz, e.g B, you must run at first cacheMat<-makeCacheMatrix(B)