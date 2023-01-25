## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # making inverse matrix NULL
 
   set <- function(y) #setting the matrix
    { x <<- y
      i <<- NULL 
    }
   
   get <- function() x # function to get the marrix
   
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
  i <- get$inverse()
  if(!is.null(i)) 
    { message("getting cached data")
      return(i) # returning the inverse
    }
  data <- x$get() #geting the matrix
  i <- solve(data, ...) #calculates inverse of matrix
  x$setinverse(i)
  i # returning the value of inverse of x
  
  }
