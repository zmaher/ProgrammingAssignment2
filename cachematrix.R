# These functions create a "makeCacheMatrix" type. 
# makeCacheMatrix takes a matrix and stores it to an environment with functions for getting and setting the inverse
# cacheSolve takes the output of makeCacheMatrix and outputs the inverse

#For proper attribution: these functions were designed through parallel with example functions 
# from the Coursera R Programming Programming Assignment 2 for cacheing the mean of a vector


## This function creates a list that stores the functions to 
## get and set a matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL #inverse will be stored to i

      #use reset the "bare" matrix insie of the makeCacheMatrix type and clear the inverse
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      #use to pull out the bare matrix from the makeCacheMatrix type
      get <- function() x
      
      #Setting and getting the inverse
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      
      #
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function takes a matrix initialized in makeCacheMatrix
## If the inverse has been cached, it returns the cached version
## Otherwise, it calculates the inverse and caches its value
## Regardless, returns the inverse of the matrix (assuming the matrix is invertible)

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      
      #If an inverse has been cached, return it
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      
      #Otherwise, calculate the inverse, set it to the cache, and return it
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      ## Return a matrix that is the inverse of 'x'
      i
}
