## A function that will create a special matrix object that can cache its inverse

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

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


##  invert the matrix but checks if the inverted matrix already exists first

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
