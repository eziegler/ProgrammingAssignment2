## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix makes a special "matrix" that is really a list containing a function to
# 1. Set the value of a matrix
# 2. Get the value of a matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <-function() x
  setinverse <- function( finalInv ) inverse <<- finalInv
  getinverse <- function() inverse
  list( set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# cacheSolve calculates the inverse of a matrix, and sets the inverse value of the matrix in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
				
		inverse <- x$getinverse()

			if(!is.null(inverse)){
					message("getting cached data")
					return(inverse)
				}

		data <- x$get()
		inverse <- solve(data)
		x$setinverse(inverse)

		inverse
}
