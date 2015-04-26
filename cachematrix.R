## The below functions cache previously run functions, output an inverted matrix of an inputted matrix, 
## and first look at cached matrices before calculation a new inverse. 

##setwd("C:/Users/Nick/Desktop/R/assignment 2")

## This first function creates a matrix of specified size and substance, and gets the values in the matrix.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function (y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function finds the inverse of the created matrix. It first looks at cached matrices before calculating
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      return(m)
      ## Returns a matrix that is the inverse of the original input.
}

##x <- makeCacheMatrix(matrix(c(6,2,3,6),2,2))
##cacheSolve(x)
