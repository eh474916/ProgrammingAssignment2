
#The makeCacheMatrix function contains a list of other functions #that 
#takes a matrix as an argument and stores that matrix as x and its #inverse as Null
#so it can be accessed the next time its called

makeCacheMatrix<-function(x=matrix()){
  i<-NULL  
  set_matrix<-function(y){
    x<<-y
    i<<-NULL   
  }
  get_matrix<-function(){
    return(x)
  }
  set_inverse<-function(inverse){
    i<<-inverse 
  }
  get_inverse<-function() {
    return(i)
  }
  list(set_matrix=set_matrix, 
       get_matrix=get_matrix,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}



#If the inverse of a matrix has not been stored/set in the #makeCacheMatrix
#and returns Null, then CacheSolve will solve for the matrix #inverse and print 
#a matrix of the inverse and will cache it for the next time the #same matrix is called

CacheSolve<-function(x,...){
  i <-x$get_inverse()
  if(!is.null(i)){
    message("inverse was cached")
    #Solve(x)
    return(i)
  }
  actual_matrix<-x$get_matrix()
  actual_inverse<-solve(actual_matrix,...)
  x$set_inverse(actual_inverse)
  return(actual_inverse)
}