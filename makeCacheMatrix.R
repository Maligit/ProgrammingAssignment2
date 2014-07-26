## This function creates a list of setters and getters methods that can be used to 
## retrieve the inverse of a squared matrix.


## The makeCacheMatrix takes a squared matrix and create a lists of sub functions
## that can be used to set the value of the matrix based on what is being passed,
## get that matrix and similarly set and get the inverse of that matrix using
## the setinverse() and getinverse() calls. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set = function(y){
      x <<- y
      m <<- NULL
  }
  get = function() x
  ##  set the inverse of a matrix using the solve function 
  setinverse <- function(solve) m <<- solve
  
  ##  getinverse returns the inverse of a matrix
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the cacheSolve is used to get a cached version of a matrix if 
## it exists or just computes it if not. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)){
        	message("getting cached inverse")
        	return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}


