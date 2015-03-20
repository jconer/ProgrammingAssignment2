## There are two distinct functions outlined herein: makeCacheMatrix & cacheSolve.
## In its implement, it will be slightly different: "cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))" is consistent, 
## In its implement, will represent a two-part sequence:
##        mypseudoMatrix<-makeCacheMatrix(myMatrix)
##        cacheSolve(mypseudoMatrix)
## The two-part sequence will return the inverse of the matrix myMatrix, with the following details:
##        Upon calling cacheSolve with argument mypseudoMatrix, R computes the inverse matrix;
##        Afterward, when cacheSolve is called on argument mypseudoMatrix, R retrieves (without 
##            calculation) the inverse of myMatrix from "cache".
##    Suppose we have executed my2ndpseudoMatrix<-makeCacheMatrix(my2ndMatrix).
##    Calling cacheSolve(my2ndpseudoMatrix) and calling cacheSove(mypseudoMatrix), in any order, will cause
##    appropriate results, including utilization of "cached" values.
## makeCacheMatrix Function Description:
##  makeCacheMatrix defines mypseudoMatrix, which is actually a list of THREE functions (the course example
##  produced a list of FOUR functions, but the "set(Y)" function [which I have commented out in the R code
##  herebelow] was never needed for the purposes of this assignment), namely the functions 
##  "get()", "setMatInv(MatInverse)", and "getMatInv()":
##        "get()" simply outputs the argument into makeCacheMatrix, which I've called myMatrix .
##
##        "setMatInv(MatInverse)" simply assigns to the environment variable Minv (i.e. 
##        the makeCacheMatrix-environment variable Minv) the setMatInv argument MatInverse.
##
##        "getMatInv()" simply outputs the parent-environment variable Minv

makeCacheMatrix <- function(myMatrix = matrix()) {
  Minv <-NULL
  set<-function(Y){
    myMatrix<<- Y
    Minv <<-NULL
  }
  get<-function() myMatrix
  setMatInv <-function(MatInverse) Minv <<- MatInverse
  getMatInv<-function() Minv
  mypseudoMatrix<<-list(get=get,setMatInv=setMatInv,getMatInv=getMatInv)
  mypseudoMatrix
}


## cacheSolve Function Description:
##  cacheSolve takes as its argument a list of fuctions (here labeled get_setMatInv_getMatInv_of_myMatrix),
##  such as the list produced by makeCacheMatrix(myMatrix), and produces the matrix inverse (here labelled 
##  Minv) matching the makeCacheMatrix(myMatrix)-environment variable Minv) using R's matrix "solve" command.
##  Upon executing cacheSolve(get_setMatInv_getMatInv_of_myMatrix), it's checked on to assess whether there is an
##  argument-associated Minv already has a "cached" value assiged, in which case that value is simply 
##  retrieved.  Should the argument-associated Minv starts out NULL then we get via  
##  makeCacheMatrix the argument-associated myMatrix, assign myMatrix to DataMatrix, and determine the inverse
##  matrix of DataMatrix, assigning that inverse to the makeCacheMatrix(myMatrix)-environment variable Minv.
##  In the interest of succinctness, the cacheSolve argument get_setMatInv_getMatInv_of_myMatrix can in default omitted,
##  provided that cacheSolve is deriving the inverse of the myMatrix for the most recently-executed 
##  makeCacheMatrix.

cacheSolve <- function(get_setMatInv_getMatInv_of_myMatrix = mypseudoMatrix, ...) {
  ## Return a matrix that is the inverse of 'Get_SetMatInv_GetMatInv_of_myMatrix'
  Minv<- get_setMatInv_getMatInv_of_myMatrix$getMatInv()
  if(!is.null(Minv)){
    message("getting cached matrix inverse")
    return(Minv)    
  }
  DataMatrix<-get_setMatInv_getMatInv_of_myMatrix$get()
  Minv<-solve(DataMatrix,...)
  get_setMatInv_getMatInv_of_myMatrix$setMatInv(Minv)
  Minv
}