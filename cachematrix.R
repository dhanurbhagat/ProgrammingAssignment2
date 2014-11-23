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
  setinv=function(inverse) 
  {
    inv<<- inverse
  }
  getinv=function()
  {
    inv
  }
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}
#The commands below allows display the matrix inverse

cacheSolve<-function(x)
{
  inv<-x$getinv()
  
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
    x$setinv(inv)
    return(inv)
  }
}