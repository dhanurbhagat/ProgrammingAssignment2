# makeCacheMatrix is able to set and get the value of a matrix and an inverse matrix


makeCacheMatrix<- function(x = matrix())
{
  inv<-NULL
  set<-function(y)
  {
    
    x<<- y
    inv<<- NULL
  }
  
  get<-function() 
    x
  setinversemat=function(inverse) 
  {
    inv<<- inverse
  }
  getinversemat=function()
  {
    inv
  }
  list(set=set,
       get=get,
       setinversemat=setinversemat,
       getinversemat=getinversemat)
}
#The commands below allows display the matrix inverse

cacheSolve<-function(x)
{
  inv<-x$getinversemat()
  
  if(!is.null(inv))
  {
    message("getting the cached data")
    return(inv)
    # This is in case the inverse hasn't been calculated yet
    
  }
  else
    
  {
    message("Cached data not found. The inverst matrix is...")
    data<- x$get()
    inv<- solve(data)
    x$setinversemat(inv)
    return(inv)
  }
}