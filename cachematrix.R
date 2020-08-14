## The aim of this two functions is to avoid time-consuming computations. 
## In particular they are designed in a way that they compute the inverse of a matrix
## storing it in the cache, such that it can be recalled in potential 
## following computations, without computing again the inverse.  

## The following function "makeCacheMatrix" creates a special "matrix",
## i.e. a list containing a function to do the following tasks: set the 
## set of values of the matrix; get the set of values of the matrix
## set the set of values of the inverse of the matrix; get the inverted matrix. 

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set<- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) s <<- solve
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The following function "cacheSolve" computes the inverse of the matrix
## that is specified above. In particular, it firstly analyzes whether the 
## inverse of the matrix has already been computed and stored in the cache. 
## If so, the function displays the value of the matrix with the message 
## "getting cached data". Otherwise, the function computes the inverse of the
## special matrix and returns it. 

cacheSolve <- function(x, ...) {
     
        ## Return a matrix that is the inverse of 'x'
      s <- x$getinverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setinverse(s)
      s
      
}




