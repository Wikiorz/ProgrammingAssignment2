## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      if(det(x)==0){
            message("matrix no inversible")
            return(NULL)
      }
      inv = NULL
      set <- function(y){
            x<<-y
            inv<<-NULL
      }
      get <- function() x
      setinv <- function(mat) inv<<-mat
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null(inv)){
            message("getting cached matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
